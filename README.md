# match-csv

Basic command line utility for searching data inside of a CSV file

```plain
 DESCRIPTION:
  Find data inside a CSV file

 SYNTAX:
  match-csv [OPTIONS] file_name [column value]...

 OPTIONS:
  -h --help
   print program information

  -o --out-file out_file
   set the output file

  -d --delimiter character
   specify the delimiter used between each field, default is ',' (comma)

  -a --all
   show also the the data in the columns it's searching from

  -f --filter column
   remove the column from the output list (can be used more than once)

  --buffer-size size
   set the internal buffer size (default is 8KiB)
```