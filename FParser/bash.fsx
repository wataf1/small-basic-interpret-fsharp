#I "c:\\Users\\jhuber1\\documents\\visual studio 2017\\Projects\\FParser\\packages\\FParsec.1.0.3\\lib\\net40-client"
#r "FParsec.dll"
#r "FParsecCS.dll"
open FParsec.CharParsers
open FParsec
open System
open System.Collections.Generic

type Letter = char
and Digit = char
and Number = float
and Word = string
and WordList =
    Single of Word
    | WList of Word*WordList
and Assignment = 
    Eq of Word*Word
and Redirection =
    | To of Word //'>' <word> Redirect the standard output (stdout) of cmd to a file
    | From of Word //'<' <word>  Redirect the contents of the file to the standard input (stdin) of cmd.
    | NTo of Number*Word //<number> '>' <word>
    | NFrom of Number*Word //<number> '<' <word>
    | Append of Word //'>>' <word>
    | NAppend of int*Word //<number> '>>' <word>
    | HereDocIn of Word //'<<' <word> Redirect a bunch of lines to the stdin.
    | NHereDocIn of Word //<number> '<<' <word>
    | DupNumIn of Number //'<&' <number>
    | NDupNumIn of Number*Number //<number> '<&' <number>
    | DupWordIn of Word //'<&' <word>
    | NDupWordIn of Number*Word//<number> '<&' <word>
    | DupWordOut of Word //'>&' <word>
    | NDupWordOut of Number*Word//<number> '>&' <word>
    | HereDocInStripTabs of Word //'<<-' <word> redirect a bunch of lines to the stdin, stripping tabs from prefix
    | NHereDocInStripTabs of int*Word //<number> '<<-' <word>
    | CloseStdOut //'>&' '-' Close stdout.
    | NCloseOut of Number //<number> '>&' '-'
    //http://my.safaribooksonline.com/book/operating-systems-and-server-administration/unix/1565923472/syntax/lbs.appd.div.3
    //https://www.tldp.org/LDP/abs/html/io-redirection.html#IOREDIRREF
    //http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_10

type Operator =
    | Comment //#
    | Pipe //|
    | AndIf  //&&
    | OrIf //||
    | DSemi //;;
    | DLess //<<
    | DGreat //>>
    | LessAnd // <&
    | GreatAnd // >&
    | LessGreat // <>
    | DLessDash // <<-
    | Clobber // >|

type ReservedWord =
    | If //if
    | Then //then
    | Else //else
    | ElseIf //elif
    | Fi //fi
    | Do //do
    | Done //done
    | Case //case
    | Esac //esac
    | While //while
    | Until // until
    | For //for
    | Bang //!
    | LBrace //{
    | RBrace //}
    | In //in


