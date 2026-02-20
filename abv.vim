" Vim syntax file
" Language: ABV (Algorithm Based Visualizer)
" Latest Revision: 2026

if exists("b:current_syntax")
  finish
endif

syntax keyword abvKeyword Начало конец начало Конец нч кц
syntax keyword abvConditional если тогда иначе
syntax keyword abvRepeat пока для выполнить от до для_ всех в массиве диапазоне
syntax keyword abvBoolean Истина Правда Ложь

syntax keyword abvType Нат нат Цел цел Десятич дробь Лог Символ Строка массив
syntax keyword abvType Алгоритм аргументы начало конец вернуть пусть

syntax match abvNumber "\<[0-9]\+\>"
syntax match abvFloat "\<[0-9]\+\.[0-9]\+\>"

syntax region abvString start='"' end='"' skip='\\"'
syntax region abvChar start="'" end="'" skip="\\'"

syntax match abvComment "//.*$"
syntax region abvComment start="/\*" end="\*/"

syntax match abvOperator "[:\-+/*%<>=!&|]"
syntax match abvOperator ":="
syntax match abvOperator "->"
syntax match abvOperator "++"
syntax match abvOperator "--"
syntax match abvOperator "=="
syntax match abvOperator "!="
syntax match abvOperator "<="
syntax match abvOperator ">="

syntax match abvMacro "\<[а-яА-ЯёЁa-zA-Z0-9_]\+\>!"

syntax match abvMacro "!\[[а-яА-ЯёЁa-zA-Z0-9_]\+\]"

highlight default link abvMacro Type

highlight default link abvKeyword Statement
highlight default link abvConditional Number
highlight default link abvRepeat Repeat
highlight default link abvType Type
highlight default link abvBoolean Boolean
highlight default link abvNumber Number
highlight default link abvFloat Float
highlight default link abvString String
highlight default link abvChar Character
highlight default link abvOperator Operator
highlight default link abvComment Comment

let b:current_syntax = "abv"
