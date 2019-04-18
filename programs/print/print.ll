; ModuleID = 'LOL'
source_filename = "LOL"

%struct.List = type { i32, %struct.List_element*, %struct.List_element* }
%struct.List_element = type { %struct.List_element*, %struct.List_element*, i8* }

@str = private unnamed_addr constant [7 x i8] c"hello!\00", align 1

declare void @print(i8*)

declare void @println(i8*)

declare i32 @int_of_float(float)

declare float @float_of_int(i32)

declare i8* @str_of_int(i32)

declare i32 @int_of_str(i8*)

declare i8* @str_of_bool(i1)

declare i8* @str_of_float(float)

declare i8* @string_concat(i8*, i8*)

declare i32 @string_equals(i8*, i8*)

declare %struct.List* @list_init()

declare %struct.List_element* @list_append(%struct.List*, i8*)

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %env = bitcast i8* %malloccall to i8**
  store i8* null, i8** %env
  %env_p = load i8*, i8** %env
  %malloccall1 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %main = bitcast i8* %malloccall1 to { i32 ()*, i8* }*
  %tmp__ = insertvalue { i32 ()*, i8* } { i32 ()* @main, i8* null }, i8* %env_p, 1
  store { i32 ()*, i8* } %tmp__, { i32 ()*, i8* }* %main
  call void @println(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str, i32 0, i32 0))
  ret i32 0
}

declare noalias i8* @malloc(i32)
