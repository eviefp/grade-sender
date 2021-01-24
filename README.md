# grade-sender

Send multiple templated emails from a csv file using gmail.

Disclaimer: I have only tested this with gmail. It uses their default port and authentication
methods. There is no way to change them without updating the source code.

## Quickstart

Before you start, make sure you have GHC 8.10.2 & cabal-install working on your system. One easy way to get them is through [ghcup](https://www.haskell.org/ghcup/).


```
$ cabal update
$ git clone https://github.com/vladciobanu/grade-sender.git
$ cd grade-sender
$ cabal build -j
$ cabal run -- grade-sender-exe --title="Email Title" --grades=/path/to/grades.csv --template=/path/to/template.tpl
```

## Grade files CSV
The easiest way to get a grade file csv is to create a google spreadsheet with all the students. Include
any information you would like to be able to add to the email (name, group, result, partial results, etc).

Make sure that the file has headers and that the email address header is labeled `Email` (case sensitive).

Go to File -> Download As -> Comma-separated values (.csv, current sheet) and save it.

The result should resemble the `marks.csv` file in the `config` folder.

## Template file
The template file is used for the body of the email. For example,

```
Hello {{Name}} {{SurName}},

Your grade for the Logic Programming exam is {{Total}} ({{Partial}} partial + {{Exam}} exam).


Your truly,
Teacher.
```

This means that your `marks.csv` file should look like this:

```csv
Name,SurName,Total,Partial,Exam,Email
John,Smith,80,30,50,john.smith@example.com
```

## Possible issues
 If you get authentication error, but are sure the username (user@gmail.com) and password are correct,
 then you might need to [allow less secure apps to access your account](https://support.google.com/accounts/answer/6010255?hl=en).

