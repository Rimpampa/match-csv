macro_rules! args {
    (
        DESCRIPTION: [$($str:literal)+]
        OPTIONS: [$($($opt:literal)? <$long:literal> $(($($val:ident),+))?: $descr:literal do $func:expr;)*]
        ARGUMENTS: $($name:ident do $argf:expr;)* $(... $(($explain:literal))? do $iterf:expr)?
    ) => {{
        let mut args = std::env::args();

        fn get_name(exe: Option<String>) -> Option<String> {
            use std::{ffi::OsStr, path::Path};
            exe.as_ref()
                .map(Path::new)
                .and_then(Path::file_name)
                .and_then(OsStr::to_str)
                .map(String::from)
        }
        let prg_name = get_name(args.next()).unwrap();

        let mut s;
        while {
            s = args.next();
            s.as_ref().map(|s| s.starts_with('-')).unwrap_or(false)
        } {
            options!(@match (args)&s.unwrap()[1..] =>
                help: print(
                    " DESCRIPTION:\n"
                    $("  " $str "\n")+
                    "\n SYNTAX:\n  {} [OPTIONS] "
                    + concat!(
                        $(stringify!($name)),*
                        $(," ", $("[", $explain, "]",)? replace!(@tt $iterf "..."))?
                    ), {prg_name}
                )
                options: [$($($opt)? <$long> $(($($val),+))?: $descr do $func),*]
            )
        }

        if let Some(s) = s {
            let mut iter = vec![s].into_iter().chain(args);
            $($argf(iter.next().unwrap_or_else(|| exit!(
                1 => concat!(" ERROR:\n  ", stringify!($name), " is missing!")
            )));)*
            $($iterf(iter);)?
        } else {
            $($argf(args.next().unwrap_or_else(|| exit!(
                1 => concat!(" ERROR:\n  ", stringify!($name), " is missing!")
            )));)*
            $($iterf(args);)?
        }

    }};
}

macro_rules! options {
    (@match ($args:expr)$s:expr =>
        help: print($($prev:literal)* + $syntax:expr, {$($arg:expr),*})
        options: [$($($opt:literal)? <$long:literal> $(($($val:ident),+))?: $descr:literal do $func:expr),*]
    ) => {
        match $s {
            "h" | "-help" => {
                exit!(0 => concat!(
                    $($prev),*, $syntax, "\n",
                    options!(@help [
                        "h" <"help">: "print program information",
                        $($($opt)? <$long> $(($($val),+))?: $descr),*
                    ])
                ), $($arg),*);
            },

            $($($opt |)? concat!("-", $long) => $func($($(
                replace!(@expr $val $args.next().unwrap_or_else(||{
                    exit!(1 => concat!(
                        " ERROR:\n  Missing '", stringify!($val),
                        "' argument to '", $long,  "' option"
                    ));
                }))
            ),+)?)),*,

            arg => exit!(1 => " ERROR:\n  Invalid option '{}'", arg),
        }
    };

    (@help [$($($opt:literal)? <$long:literal> $(($($val:ident),+))?: $descr:literal),*]) => {
        concat!(
            "\n OPTIONS:",
            $("\n  ", $("-", $opt, " ",)? "--", $long, $(" ", $(stringify!($val), " ",)+)?"\n   ", $descr, "\n" ),*
        )
    };
}

macro_rules! replace {
    (@expr $a:tt $b:expr) => {
        $b
    };
    (@tt $a:tt $b:tt) => {
        $b
    };
}
