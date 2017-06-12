# fbforceexport
Firebird Force Export Data to CSV file

If you have database currpted, this simple tool, ver simple to export all tables you defined it in tables.txt to data folder as csv.
It not fix the database, it just bypass the error when can not fetch the data, as possible this simple tool.

Have fun :P

## Usage

To run it with embed firebird sql you need "fbembed.dll" in the same folder of exe in subfolder "d:/myproject/fb/fbembed.dll".

open new file "tables.txt" and put your tables want to export it

```
AccLists
AccObjects
AccTotals
Accounts
```

## Compile

You need minilib from https://sourceforge.net/p/minilib/