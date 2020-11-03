macro_rules! exit {
    (1 => $($t:tt)*) => {{
        eprintln!($($t)*);
        std::process::exit(1);
    }};

    (0 => $($t:tt)*) => {{
        println!($($t)*);
        std::process::exit(0);
    }};
}

#[macro_use]
mod args;

use io::Read;
use std::str::from_utf8_unchecked as as_str;
use std::{fs, io, mem, time};

macro_rules! find {
    ($($val:expr),+$(,)? => $slice:expr $(; $map:expr)?) => {
        $slice.iter()$(.map($map))?.position(|v| $(*v == $val ||)+ false)
    }
}

#[derive(Debug)]
enum Error {
    NeedMoreData,
    TooManyValues,
    NotEnoughValues,
}

type Outcome<T> = Result<T, Error>;

#[derive(PartialEq, Eq)]
enum Stage {
    ParseHeader,
    ParseData,
}

fn main() {
    let mut out_file = None;
    let mut delimiter = b',';
    let mut in_file = mem::MaybeUninit::uninit();
    let mut all = false;
    let mut filter = Vec::new();
    let mut buffer_size = 8192; // 1KiB

    let mut columns = Vec::new();
    let mut values = Vec::new();

    args! {
        DESCRIPTION: ["Find data inside a CSV file"]
        OPTIONS: [
            "o" <"out-file"> (out_file): "set the output file"
                do |file| match fs::File::create(&file) {
                    Ok(file) => out_file = Some(file),
                    Err(err) => exit!(1 => " ERROR:\n  Cannot open '{}': {}", file, err),
                };

            "d" <"delimiter"> (character):
                "specify the delimiter used between each field, default is ',' (comma)"

                do |d: String| if d.len() > 1 {
                    exit!(1 => " ERROR:\n  The 'd' option expects only one character");
                } else {
                    delimiter = d.chars().next().unwrap() as u8
                };

            "a" <"all">: "show also the the data in the columns it's searching from"
                do || all = true;

            "f" <"filter"> (column):
                "remove the column from the output list (can be used more than once)"

                do |column: String| filter.push(column.into_bytes());

            <"buffer-size"> (size):
                "set the internal buffer size (default is 8KiB)"
                do |size: String| match size.parse::<usize>() {
                    Ok(size) if buffer_size >= 1024 => buffer_size = size,
                    Err(err) => exit!(1 => " ERROR:\n  Buffer size options is not a number: {}", err),
                    Ok(_) => exit!(1 => " ERROR:\n  Buffer size must be greater than 1024"),
                };
        ]
        ARGUMENTS:
            file_name do |file| match fs::File::open(&file) {
                Ok(file) => in_file = mem::MaybeUninit::new(file),
                Err(err) => exit!(1 => " ERROR:\n  Cannot open '{}': {}", file, err),
            };
            ... ("column value") do |iter| {
                // Function is used to accept a generic iterator value
                fn collect<T: Iterator<Item = String>>(iter: T) -> [Vec<String>; 2] {
                    let mut a = Vec::new();
                    let mut b = Vec::new();
                    for (i, item) in iter.enumerate() {
                        match i & 1 == 0 {
                            true => &mut a,
                            false => &mut b,
                        }.push(item);
                    }
                    if a.len() > b.len() {
                        exit!(1 => " ERROR:\n  Missing value for column '{}'", a.last().unwrap());
                    }
                    [a, b]
                }
                let [a, b] = collect(iter);
                columns = a.into_iter().map(String::into_bytes).collect();
                values = b.into_iter().map(String::into_bytes).collect();
            }
    }
    // SAFETY: the program ends before it arrives here if the file isn't opened
    let mut in_file = unsafe { in_file.assume_init() };

    let mut buffer = vec![0u8; buffer_size];
    let mut available = 0;
    let mut at = 0;

    // column index => value index
    let mut pairs: Vec<(usize, usize)> = Vec::with_capacity(columns.len());
    // indices of the fields that shouldn't be displayed
    let mut remove: Vec<usize> = if all {
        Vec::with_capacity(filter.len())
    } else {
        // Count the fields that apper on both the selected columns
        // and the filtered columns
        let common = filter
            .iter()
            .filter(|&v| find!(*v => columns).is_some())
            .count();
        Vec::with_capacity(filter.len() - common)
    };
    let mut fields = Vec::new();
    // number of fields of the file
    let mut cols_number = 0;
    let mut stage = Stage::ParseHeader;
    let mut matches: Vec<Vec<String>> = Vec::new();
    let mut row = 0;

    let start = time::Instant::now();

    while {
        // if the available data fills the whole buffer, there is no more
        // space to put new data, thus the buffer is too small
        if available == buffer_size {
            exit!(1 => "Buffer size is too small for this file (current is: {})", buffer_size);
        }
        let bytes = in_file.read(&mut buffer[at + available..]).unwrap();
        available += bytes;
        bytes > 0
    } {
        if stage == Stage::ParseHeader {
            match get_header(&buffer[at..at + available], delimiter) {
                Ok(mut vec) => {
                    let new: usize = vec.iter().map(|v| v.len() + 1).sum();
                    if available == new || (buffer[at + new - 1] == b'\r' && available < 1) {
                        // Too Little Data
                        buffer.copy_within(at.., 0);
                        at = 0;
                    } else {
                        for (column, &field) in vec.iter().enumerate() {
                            // Search the field name in the selected columns list
                            if let Some(idx) = find!(field => columns) {
                                // If it's filtered add it's index to the list
                                if !all || find!(field => filter).is_some() {
                                    remove.push(column);
                                }
                                // Check that it didn't appear more than once
                                match find!(idx => pairs; |(i, _)| i) {
                                    Some(_) => exit!(1 =>
                                        " ERROR:\n  The column '{}' appears more than once in the file!",
                                        unsafe { as_str(&columns[idx]) }
                                    ),
                                    None => pairs.push((column, idx)),
                                }
                            }
                            // Search the field name in the columns to filter list
                            else if find!(field => filter).is_some() {
                                remove.push(column);
                            }
                        }
                        cols_number = vec.len();
                        // Remove the filtered columns
                        for (n, i) in remove.iter().enumerate() {
                            vec.remove(*i - n);
                        }
                        fields = unsafe { to_string_unchecked(&vec) };

                        // Check that all the selected columns have been found
                        if pairs.len() < columns.len() {
                            if let Some(idx) = (0..columns.len())
                                .find(|&idx| find!(idx => pairs; |(_, i)| i) == None)
                            {
                                exit!(1 => " ERROR:\n  Cannot find column '{}'!", unsafe {
                                    as_str(&columns[idx])
                                })
                            }
                        }
                        if buffer[at + new - 1] == b'\r' {
                            at += new + 1;
                            available -= new + 1;
                        } else {
                            at += new;
                            available -= new;
                        }
                        row += 1;
                        stage = Stage::ParseData;
                    }
                }
                Err(Error::NeedMoreData) => {
                    buffer.copy_within(at.., 0);
                    at = 0;
                }
                Err(Error::TooManyValues) | Err(Error::NotEnoughValues) => unreachable!(),
            }
        }
        if stage == Stage::ParseData {
            let mut outcome;
            'parse_data: while {
                outcome = get_fields(&buffer[at..at + available], cols_number, delimiter);
                outcome.is_ok()
            } {
                let mut vec: Vec<&[u8]> = outcome.unwrap();
                let new: usize = vec.iter().map(|v| v.len() + 1).sum();
                if available == new {
                    outcome = Err(Error::NeedMoreData);
                    break 'parse_data;
                }
                if buffer[at + new - 1] == b'\r' {
                    if available < 1 {
                        outcome = Err(Error::NeedMoreData);
                        break 'parse_data;
                    }
                    at += 1;
                    available -= 1;
                }
                // Check if it matches
                if pairs.iter().all(|&(col, val)| vec[col] == values[val]) {
                    // If it does, remove the filtered columns
                    for (n, i) in remove.iter().enumerate() {
                        vec.remove(*i - n);
                    }
                    let str_vec = unsafe { to_string_unchecked(&vec) };
                    matches.push(str_vec);
                }
                at += new;
                available -= new;
                row += 1;
            }

            match outcome {
                Ok(_) => unreachable!(),
                Err(Error::NeedMoreData) => {
                    buffer.copy_within(at.., 0);
                    at = 0;
                }
                Err(Error::TooManyValues) => exit!(1 =>
                    " ERROR:\n  The row #{} has more than {} fields",
                    row + 1,
                    cols_number,
                ),
                Err(Error::NotEnoughValues) => exit!(1 =>
                    " ERROR:\n  The row #{} has less than {} fields",
                    row + 1,
                    cols_number,
                ),
            }
        }
    }
    let end = start.elapsed();

    let print_start = time::Instant::now();
    // If you want to use this code for other purposes just remove this part
    // and do whatever you want with the `matches`
    if let Some(ref mut file) = out_file {
        let mut writer = io::BufWriter::with_capacity(buffer_size, file);
        if let Err(err) = print_table(&mut writer, &matches, &fields, row + 1) {
            exit!(1 => " ERROR:\n  Writing to the output file failed with: {}", err);
        }
    } else {
        print_table(&mut io::stdout().lock(), &matches, &fields, row + 1).unwrap();
    }
    let print_end = print_start.elapsed();

    eprintln!("Executed in: {}s", start.elapsed().as_secs_f32());
    eprintln!("Search elapsed: {}s", end.as_secs_f32());
    eprintln!("Printing elaped: {}s", print_end.as_secs_f32());
}

fn print_table<W: io::Write>(
    writer: &mut W,
    matches: &[Vec<String>],
    fields: &[String],
    rows: usize,
) -> io::Result<()> {
    let mut max = Vec::with_capacity(fields.len());
    max.extend(fields.iter().map(|v| v.len()));

    for values in matches {
        for (max, value) in max.iter_mut().zip(values.iter()) {
            if *max < value.len() {
                *max = value.len();
            }
        }
    }
    let mut separator = String::with_capacity(max.iter().sum::<usize>() + max.len() + 1);
    separator.push('+');
    for &m in &max {
        separator.extend((0..m + 2).map(|_| '-'));
        separator.push('+');
    }

    writeln!(writer, "{}", separator)?;
    for (field, m) in fields.iter().zip(max.iter()) {
        write!(writer, "| {:^1$} ", field, m)?;
    }
    writeln!(writer, "|\n{}", separator)?;

    for values in matches {
        for (value, m) in values.iter().zip(max.iter()) {
            write!(writer, "| {:1$} ", value, m)?;
        }
        writeln!(writer, "|")?;
    }
    writeln!(writer, "{}", separator)?;
    writeln!(
        writer,
        "Total nubmer of rows: {}\nMatched rows: {}",
        rows,
        matches.len()
    )?;
    Ok(())
}

fn skip_field(slice: &[u8], del: u8) -> Outcome<usize> {
    let mut at = 0;
    while {
        at += find!(del, b'\r', b'\n', b'\\', b'"' => slice[at..]).ok_or(Error::NeedMoreData)?;
        slice[at] != del && slice[at] != b'\n' && slice[at] != b'\r'
    } {
        if slice[at] == b'\\' {
            at += 1;
        } else if slice[at] == b'"' {
            if at + 1 >= slice.len() {
                return Err(Error::NeedMoreData);
            }
            at += find!(b'"' => slice[at + 1..]).ok_or(Error::NeedMoreData)? + 1;
        }
        at += 1;
        if at >= slice.len() {
            return Err(Error::NeedMoreData);
        }
    }
    Ok(at)
}

fn get_fields(slice: &[u8], fields: usize, del: u8) -> Outcome<Vec<&[u8]>> {
    let mut vec: Vec<&[u8]> = Vec::with_capacity(fields);
    let mut at = 0;
    while {
        let len = skip_field(&slice[at..], del)?;
        let field = &slice[at..at + len];
        if vec.len() == fields {
            return Err(Error::TooManyValues);
        }
        vec.push(field);

        at += len;
        slice[at] != b'\n' && slice[at] != b'\r'
    } {
        at += 1;
        if at >= slice.len() {
            return Err(Error::NeedMoreData);
        }
    }
    match vec.len() < fields {
        true => Err(Error::NotEnoughValues),
        false => Ok(vec),
    }
}

fn get_header(slice: &[u8], del: u8) -> Outcome<Vec<&[u8]>> {
    let mut vec = Vec::new();
    let mut at = 0;
    while {
        let len = skip_field(&slice[at..], del)?;
        let field = &slice[at..at + len];
        vec.push(field);

        at += len;
        slice[at] != b'\n' && slice[at] != b'\r'
    } {
        at += 1;
        if at >= slice.len() {
            return Err(Error::NeedMoreData);
        }
    }
    Ok(vec)
}

unsafe fn to_string_unchecked(vec: &[&[u8]]) -> Vec<String> {
    let mut new = Vec::with_capacity(vec.len());
    new.extend(vec.iter().map(|arr| as_str(arr)).map(String::from));
    new
}
