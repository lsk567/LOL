; ModuleID = 'LOL'
source_filename = "LOL"

@str = private unnamed_addr constant [13 x i8] c"Hello World!\00", align 1

declare void @gsl_test(i8*)

declare void @printhw(i8*)

declare void @printnewln(i8*)

declare void @println(i8*)

declare void @print(i8*)

declare i32 @int_of_float(float)

declare float @float_of_int(i32)

declare i8* @str_of_int(i32)

declare i32 @int_of_str(i8*)

declare i8* @str_of_bool(i1)

declare i8* @str_of_float(float)

declare i8* @string_concat(i8*, i8*)

declare i32 @string_equals(i8*, i8*)

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
  %malloccall2 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %printHelloWorld = bitcast i8* %malloccall2 to { void (i8*)*, i8* }*
  %malloccall3 = tail call i8* @malloc(i32 0)
  %tmp_ = bitcast i8* %malloccall3 to {}*
  store {} zeroinitializer, {}* %tmp_
  %env_p4 = bitcast {}* %tmp_ to i8*
  %tmp__5 = insertvalue { void (i8*)*, i8* } { void (i8*)* @f0, i8* null }, i8* %env_p4, 1
  store { void (i8*)*, i8* } %tmp__5, { void (i8*)*, i8* }* %printHelloWorld
  %printHelloWorld6 = load { void (i8*)*, i8* }, { void (i8*)*, i8* }* %printHelloWorld
  %fp = extractvalue { void (i8*)*, i8* } %printHelloWorld6, 0
  %envp = extractvalue { void (i8*)*, i8* } %printHelloWorld6, 1
  call void %fp(i8* %envp)
  ret i32 0
}

define void @f0(i8* %env) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %env1 = bitcast i8* %malloccall to i8**
  store i8* %env, i8** %env1
  %env_p = load i8*, i8** %env1
  %malloccall2 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %printHelloWorld = bitcast i8* %malloccall2 to { void (i8*)*, i8* }*
  %tmp__ = insertvalue { void (i8*)*, i8* } { void (i8*)* @f0, i8* null }, i8* %env_p, 1
  store { void (i8*)*, i8* } %tmp__, { void (i8*)*, i8* }* %printHelloWorld
  call void @printhw(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @str, i32 0, i32 0))
  ret void
}

declare noalias i8* @malloc(i32)
