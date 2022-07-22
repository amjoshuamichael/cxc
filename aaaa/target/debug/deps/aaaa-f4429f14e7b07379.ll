; ModuleID = '4n8hqx8i84m93xkw'
source_filename = "4n8hqx8i84m93xkw"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.7.0"

%"core::panic::location::Location" = type { { [0 x i8]*, i64 }, i32, i32 }
%"unwind::libunwind::_Unwind_Exception" = type { i64, void (i32, %"unwind::libunwind::_Unwind_Exception"*)*, [6 x i64] }
%"unwind::libunwind::_Unwind_Context" = type { [0 x i8] }

@vtable.0 = private unnamed_addr constant <{ i8*, [16 x i8], i8*, i8*, i8* }> <{ i8* bitcast (void (i64**)* @"_ZN4core3ptr85drop_in_place$LT$std..rt..lang_start$LT$$LP$$RP$$GT$..$u7b$$u7b$closure$u7d$$u7d$$GT$17ha14e469f7e4e7619E" to i8*), [16 x i8] c"\08\00\00\00\00\00\00\00\08\00\00\00\00\00\00\00", i8* bitcast (i32 (i64**)* @"_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h0e79ae1531ace3c1E" to i8*), i8* bitcast (i32 (i64**)* @"_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h37f8371ecb54c99dE" to i8*), i8* bitcast (i32 (i64**)* @"_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h37f8371ecb54c99dE" to i8*) }>, align 8, !dbg !0
@alloc10 = private unnamed_addr constant <{ [11 x i8] }> <{ [11 x i8] c"src/main.rs" }>, align 1
@alloc11 = private unnamed_addr constant <{ i8*, [16 x i8] }> <{ i8* getelementptr inbounds (<{ [11 x i8] }>, <{ [11 x i8] }>* @alloc10, i32 0, i32 0, i32 0), [16 x i8] c"\0B\00\00\00\00\00\00\00\0E\00\00\00\0D\00\00\00" }>, align 8
@str.1 = internal constant [33 x i8] c"attempt to subtract with overflow"

; std::sys_common::backtrace::__rust_begin_short_backtrace
; Function Attrs: noinline uwtable
define internal void @_ZN3std10sys_common9backtrace28__rust_begin_short_backtrace17h9843a81026573baeE(void ()* %f) unnamed_addr #0 personality i32 (i32, i32, i64, %"unwind::libunwind::_Unwind_Exception"*, %"unwind::libunwind::_Unwind_Context"*)* @rust_eh_personality !dbg !31 {
start:
  %0 = alloca { i8*, i32 }, align 8
  %f.dbg.spill = alloca void ()*, align 8
  %result.dbg.spill = alloca {}, align 1
  call void @llvm.dbg.declare(metadata {}* %result.dbg.spill, metadata !39, metadata !DIExpression()), !dbg !44
  store void ()* %f, void ()** %f.dbg.spill, align 8
  call void @llvm.dbg.declare(metadata void ()** %f.dbg.spill, metadata !38, metadata !DIExpression()), !dbg !45
; call core::ops::function::FnOnce::call_once
  call void @_ZN4core3ops8function6FnOnce9call_once17he75420568aee3710E(void ()* %f), !dbg !46
  br label %bb1, !dbg !46

bb1:                                              ; preds = %start
; invoke core::hint::black_box
  invoke void @_ZN4core4hint9black_box17hec351bf934961c38E()
          to label %bb2 unwind label %cleanup, !dbg !47

bb3:                                              ; preds = %cleanup
  br label %bb4, !dbg !48

cleanup:                                          ; preds = %bb1
  %1 = landingpad { i8*, i32 }
          cleanup
  %2 = extractvalue { i8*, i32 } %1, 0
  %3 = extractvalue { i8*, i32 } %1, 1
  %4 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %0, i32 0, i32 0
  store i8* %2, i8** %4, align 8
  %5 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %0, i32 0, i32 1
  store i32 %3, i32* %5, align 8
  br label %bb3

bb2:                                              ; preds = %bb1
  ret void, !dbg !49

bb4:                                              ; preds = %bb3
  %6 = bitcast { i8*, i32 }* %0 to i8**, !dbg !50
  %7 = load i8*, i8** %6, align 8, !dbg !50
  %8 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %0, i32 0, i32 1, !dbg !50
  %9 = load i32, i32* %8, align 8, !dbg !50
  %10 = insertvalue { i8*, i32 } undef, i8* %7, 0, !dbg !50
  %11 = insertvalue { i8*, i32 } %10, i32 %9, 1, !dbg !50
  resume { i8*, i32 } %11, !dbg !50
}

; std::rt::lang_start
; Function Attrs: uwtable
define hidden i64 @_ZN3std2rt10lang_start17h5489c8da79f3ee56E(void ()* %main, i64 %argc, i8** %argv) unnamed_addr #1 !dbg !51 {
start:
  %v.dbg.spill = alloca i64, align 8
  %argv.dbg.spill = alloca i8**, align 8
  %argc.dbg.spill = alloca i64, align 8
  %main.dbg.spill = alloca void ()*, align 8
  %_8 = alloca i64*, align 8
  %_4 = alloca i64, align 8
  store void ()* %main, void ()** %main.dbg.spill, align 8
  call void @llvm.dbg.declare(metadata void ()** %main.dbg.spill, metadata !60, metadata !DIExpression()), !dbg !66
  store i64 %argc, i64* %argc.dbg.spill, align 8
  call void @llvm.dbg.declare(metadata i64* %argc.dbg.spill, metadata !61, metadata !DIExpression()), !dbg !67
  store i8** %argv, i8*** %argv.dbg.spill, align 8
  call void @llvm.dbg.declare(metadata i8*** %argv.dbg.spill, metadata !62, metadata !DIExpression()), !dbg !68
  %0 = bitcast i64** %_8 to void ()**, !dbg !69
  store void ()* %main, void ()** %0, align 8, !dbg !69
  %_5.0 = bitcast i64** %_8 to {}*, !dbg !70
; call std::rt::lang_start_internal
  %1 = call i64 @_ZN3std2rt19lang_start_internal17h64d521fd33e3484cE({}* align 1 %_5.0, [3 x i64]* align 8 bitcast (<{ i8*, [16 x i8], i8*, i8*, i8* }>* @vtable.0 to [3 x i64]*), i64 %argc, i8** %argv), !dbg !71
  store i64 %1, i64* %_4, align 8, !dbg !71
  br label %bb1, !dbg !71

bb1:                                              ; preds = %start
  %v = load i64, i64* %_4, align 8, !dbg !72
  store i64 %v, i64* %v.dbg.spill, align 8, !dbg !72
  call void @llvm.dbg.declare(metadata i64* %v.dbg.spill, metadata !63, metadata !DIExpression()), !dbg !73
  ret i64 %v, !dbg !74
}

; std::rt::lang_start::{{closure}}
; Function Attrs: inlinehint uwtable
define internal i32 @"_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h37f8371ecb54c99dE"(i64** align 8 %_1) unnamed_addr #2 !dbg !75 {
start:
  %_1.dbg.spill = alloca i64**, align 8
  store i64** %_1, i64*** %_1.dbg.spill, align 8
  %0 = load i64**, i64*** %_1.dbg.spill, align 8, !nonnull !23, !align !82, !noundef !23
  %1 = bitcast i64** %0 to void ()**
  call void @llvm.dbg.declare(metadata i64*** %_1.dbg.spill, metadata !81, metadata !DIExpression(DW_OP_deref)), !dbg !83
  %2 = bitcast i64** %_1 to void ()**, !dbg !84
  %_4 = load void ()*, void ()** %2, align 8, !dbg !84, !nonnull !23, !noundef !23
; call std::sys_common::backtrace::__rust_begin_short_backtrace
  call void @_ZN3std10sys_common9backtrace28__rust_begin_short_backtrace17h9843a81026573baeE(void ()* %_4), !dbg !85
  br label %bb1, !dbg !85

bb1:                                              ; preds = %start
; call <() as std::process::Termination>::report
  %_2 = call i8 @"_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17hd12a6c7762e273c4E"(), !dbg !85
  br label %bb2, !dbg !85

bb2:                                              ; preds = %bb1
; call std::process::ExitCode::to_i32
  %3 = call i32 @_ZN3std7process8ExitCode6to_i3217h8d7d08954b0dbf9dE(i8 %_2), !dbg !85
  br label %bb3, !dbg !85

bb3:                                              ; preds = %bb2
  ret i32 %3, !dbg !86
}

; std::sys::unix::process::process_common::ExitCode::as_i32
; Function Attrs: inlinehint uwtable
define internal i32 @_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h74e27ed49fb0fb0bE(i8* align 1 %self) unnamed_addr #2 !dbg !87 {
start:
  %self.dbg.spill = alloca i8*, align 8
  store i8* %self, i8** %self.dbg.spill, align 8
  call void @llvm.dbg.declare(metadata i8** %self.dbg.spill, metadata !100, metadata !DIExpression()), !dbg !101
  %_2 = load i8, i8* %self, align 1, !dbg !102
  %0 = zext i8 %_2 to i32, !dbg !102
  ret i32 %0, !dbg !103
}

; std::process::ExitCode::to_i32
; Function Attrs: inlinehint uwtable
define internal i32 @_ZN3std7process8ExitCode6to_i3217h8d7d08954b0dbf9dE(i8 %0) unnamed_addr #2 !dbg !104 {
start:
  %self = alloca i8, align 1
  store i8 %0, i8* %self, align 1
  call void @llvm.dbg.declare(metadata i8* %self, metadata !113, metadata !DIExpression()), !dbg !114
; call std::sys::unix::process::process_common::ExitCode::as_i32
  %1 = call i32 @_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h74e27ed49fb0fb0bE(i8* align 1 %self), !dbg !115
  br label %bb1, !dbg !115

bb1:                                              ; preds = %start
  ret i32 %1, !dbg !116
}

; core::ops::function::FnOnce::call_once{{vtable.shim}}
; Function Attrs: inlinehint uwtable
define internal i32 @"_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h0e79ae1531ace3c1E"(i64** %_1) unnamed_addr #2 !dbg !117 {
start:
  %_1.dbg.spill = alloca i64**, align 8
  %_2 = alloca {}, align 1
  store i64** %_1, i64*** %_1.dbg.spill, align 8
  call void @llvm.dbg.declare(metadata i64*** %_1.dbg.spill, metadata !127, metadata !DIExpression()), !dbg !132
  call void @llvm.dbg.declare(metadata {}* %_2, metadata !128, metadata !DIExpression()), !dbg !132
  %0 = load i64*, i64** %_1, align 8, !dbg !132, !nonnull !23, !noundef !23
; call core::ops::function::FnOnce::call_once
  %1 = call i32 @_ZN4core3ops8function6FnOnce9call_once17h24629aabe3434f74E(i64* %0), !dbg !132
  br label %bb1, !dbg !132

bb1:                                              ; preds = %start
  ret i32 %1, !dbg !132
}

; core::ops::function::FnOnce::call_once
; Function Attrs: inlinehint uwtable
define internal i32 @_ZN4core3ops8function6FnOnce9call_once17h24629aabe3434f74E(i64* %0) unnamed_addr #2 personality i32 (i32, i32, i64, %"unwind::libunwind::_Unwind_Exception"*, %"unwind::libunwind::_Unwind_Context"*)* @rust_eh_personality !dbg !133 {
start:
  %1 = alloca { i8*, i32 }, align 8
  %_2 = alloca {}, align 1
  %_1 = alloca i64*, align 8
  store i64* %0, i64** %_1, align 8
  call void @llvm.dbg.declare(metadata i64** %_1, metadata !137, metadata !DIExpression()), !dbg !139
  call void @llvm.dbg.declare(metadata {}* %_2, metadata !138, metadata !DIExpression()), !dbg !139
; invoke std::rt::lang_start::{{closure}}
  %2 = invoke i32 @"_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h37f8371ecb54c99dE"(i64** align 8 %_1)
          to label %bb1 unwind label %cleanup, !dbg !139

bb3:                                              ; preds = %cleanup
  br label %bb4, !dbg !139

cleanup:                                          ; preds = %start
  %3 = landingpad { i8*, i32 }
          cleanup
  %4 = extractvalue { i8*, i32 } %3, 0
  %5 = extractvalue { i8*, i32 } %3, 1
  %6 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %1, i32 0, i32 0
  store i8* %4, i8** %6, align 8
  %7 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %1, i32 0, i32 1
  store i32 %5, i32* %7, align 8
  br label %bb3

bb1:                                              ; preds = %start
  br label %bb2, !dbg !139

bb4:                                              ; preds = %bb3
  %8 = bitcast { i8*, i32 }* %1 to i8**, !dbg !139
  %9 = load i8*, i8** %8, align 8, !dbg !139
  %10 = getelementptr inbounds { i8*, i32 }, { i8*, i32 }* %1, i32 0, i32 1, !dbg !139
  %11 = load i32, i32* %10, align 8, !dbg !139
  %12 = insertvalue { i8*, i32 } undef, i8* %9, 0, !dbg !139
  %13 = insertvalue { i8*, i32 } %12, i32 %11, 1, !dbg !139
  resume { i8*, i32 } %13, !dbg !139

bb2:                                              ; preds = %bb1
  ret i32 %2, !dbg !139
}

; core::ops::function::FnOnce::call_once
; Function Attrs: inlinehint uwtable
define internal void @_ZN4core3ops8function6FnOnce9call_once17he75420568aee3710E(void ()* %_1) unnamed_addr #2 !dbg !140 {
start:
  %_1.dbg.spill = alloca void ()*, align 8
  %_2 = alloca {}, align 1
  store void ()* %_1, void ()** %_1.dbg.spill, align 8
  call void @llvm.dbg.declare(metadata void ()** %_1.dbg.spill, metadata !142, metadata !DIExpression()), !dbg !146
  call void @llvm.dbg.declare(metadata {}* %_2, metadata !143, metadata !DIExpression()), !dbg !146
  call void %_1(), !dbg !146
  br label %bb1, !dbg !146

bb1:                                              ; preds = %start
  ret void, !dbg !146
}

; core::ptr::drop_in_place<std::rt::lang_start<()>::{{closure}}>
; Function Attrs: inlinehint uwtable
define internal void @"_ZN4core3ptr85drop_in_place$LT$std..rt..lang_start$LT$$LP$$RP$$GT$..$u7b$$u7b$closure$u7d$$u7d$$GT$17ha14e469f7e4e7619E"(i64** %_1) unnamed_addr #2 !dbg !147 {
start:
  %_1.dbg.spill = alloca i64**, align 8
  store i64** %_1, i64*** %_1.dbg.spill, align 8
  call void @llvm.dbg.declare(metadata i64*** %_1.dbg.spill, metadata !153, metadata !DIExpression()), !dbg !156
  ret void, !dbg !156
}

; core::hint::black_box
; Function Attrs: inlinehint uwtable
define internal void @_ZN4core4hint9black_box17hec351bf934961c38E() unnamed_addr #2 !dbg !157 {
start:
  %dummy.dbg.spill = alloca {}, align 1
  call void @llvm.dbg.declare(metadata {}* %dummy.dbg.spill, metadata !163, metadata !DIExpression()), !dbg !164
  call void asm sideeffect "", "r,~{memory}"({}* undef), !dbg !165, !srcloc !166
  br label %bb1, !dbg !165

bb1:                                              ; preds = %start
  ret void, !dbg !167
}

; <() as std::process::Termination>::report
; Function Attrs: inlinehint uwtable
define internal i8 @"_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17hd12a6c7762e273c4E"() unnamed_addr #2 !dbg !168 {
start:
  %self.dbg.spill = alloca {}, align 1
  call void @llvm.dbg.declare(metadata {}* %self.dbg.spill, metadata !173, metadata !DIExpression()), !dbg !174
  ret i8 0, !dbg !175
}

; aaaa::main
; Function Attrs: uwtable
define internal void @_ZN4aaaa4main17h3a8ac27ca49c8f71E() unnamed_addr #1 !dbg !176 {
start:
  %z.dbg.spill = alloca i64, align 8
  %y_ptr.dbg.spill = alloca i64, align 8
  %0 = alloca i64, align 8
  %x_ptr.dbg.spill = alloca i64, align 8
  %1 = alloca i64, align 8
  %x = alloca { i32, i32 }, align 4
  call void @llvm.dbg.declare(metadata { i32, i32 }* %x, metadata !180, metadata !DIExpression()), !dbg !192
  %2 = bitcast { i32, i32 }* %x to i32*, !dbg !193
  store i32 128, i32* %2, align 4, !dbg !193
  %3 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %x, i32 0, i32 1, !dbg !193
  store i32 128, i32* %3, align 4, !dbg !193
  %_3 = bitcast { i32, i32 }* %x to i32*, !dbg !194
  %4 = bitcast i64* %1 to i32**, !dbg !195
  store i32* %_3, i32** %4, align 8, !dbg !195
  %x_ptr = load i64, i64* %1, align 8, !dbg !195
  store i64 %x_ptr, i64* %x_ptr.dbg.spill, align 8, !dbg !195
  call void @llvm.dbg.declare(metadata i64* %x_ptr.dbg.spill, metadata !186, metadata !DIExpression()), !dbg !196
  br label %bb1, !dbg !195

bb1:                                              ; preds = %start
  %_5 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %x, i32 0, i32 1, !dbg !197
  %5 = bitcast i64* %0 to i32**, !dbg !198
  store i32* %_5, i32** %5, align 8, !dbg !198
  %y_ptr = load i64, i64* %0, align 8, !dbg !198
  store i64 %y_ptr, i64* %y_ptr.dbg.spill, align 8, !dbg !198
  call void @llvm.dbg.declare(metadata i64* %y_ptr.dbg.spill, metadata !188, metadata !DIExpression()), !dbg !199
  br label %bb2, !dbg !198

bb2:                                              ; preds = %bb1
  %6 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %y_ptr, i64 %x_ptr), !dbg !200
  %_9.0 = extractvalue { i64, i1 } %6, 0, !dbg !200
  %_9.1 = extractvalue { i64, i1 } %6, 1, !dbg !200
  %7 = call i1 @llvm.expect.i1(i1 %_9.1, i1 false), !dbg !200
  br i1 %7, label %panic, label %bb3, !dbg !200

bb3:                                              ; preds = %bb2
  store i64 %_9.0, i64* %z.dbg.spill, align 8, !dbg !200
  call void @llvm.dbg.declare(metadata i64* %z.dbg.spill, metadata !190, metadata !DIExpression()), !dbg !201
  ret void, !dbg !202

panic:                                            ; preds = %bb2
; call core::panicking::panic
  call void @_ZN4core9panicking5panic17hdf567e6d4bb7f667E([0 x i8]* align 1 bitcast ([33 x i8]* @str.1 to [0 x i8]*), i64 33, %"core::panic::location::Location"* align 8 bitcast (<{ i8*, [16 x i8] }>* @alloc11 to %"core::panic::location::Location"*)) #7, !dbg !200
  unreachable, !dbg !200
}

; Function Attrs: uwtable
declare i32 @rust_eh_personality(i32, i32, i64, %"unwind::libunwind::_Unwind_Exception"*, %"unwind::libunwind::_Unwind_Context"*) unnamed_addr #1

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #3

; std::rt::lang_start_internal
; Function Attrs: uwtable
declare i64 @_ZN3std2rt19lang_start_internal17h64d521fd33e3484cE({}* align 1, [3 x i64]* align 8, i64, i8**) unnamed_addr #1

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #3

; Function Attrs: nofree nosync nounwind readnone willreturn
declare i1 @llvm.expect.i1(i1, i1) #4

; core::panicking::panic
; Function Attrs: cold noinline noreturn uwtable
declare void @_ZN4core9panicking5panic17hdf567e6d4bb7f667E([0 x i8]* align 1, i64, %"core::panic::location::Location"* align 8) unnamed_addr #5

define i32 @main(i32 %0, i8** %1) unnamed_addr #6 {
top:
  %2 = sext i32 %0 to i64
; call std::rt::lang_start
  %3 = call i64 @_ZN3std2rt10lang_start17h5489c8da79f3ee56E(void ()* @_ZN4aaaa4main17h3a8ac27ca49c8f71E, i64 %2, i8** %1)
  %4 = trunc i64 %3 to i32
  ret i32 %4
}

attributes #0 = { noinline uwtable "frame-pointer"="all" "probe-stack"="__rust_probestack" "target-cpu"="core2" }
attributes #1 = { uwtable "frame-pointer"="all" "probe-stack"="__rust_probestack" "target-cpu"="core2" }
attributes #2 = { inlinehint uwtable "frame-pointer"="all" "probe-stack"="__rust_probestack" "target-cpu"="core2" }
attributes #3 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #4 = { nofree nosync nounwind readnone willreturn }
attributes #5 = { cold noinline noreturn uwtable "frame-pointer"="all" "probe-stack"="__rust_probestack" "target-cpu"="core2" }
attributes #6 = { "frame-pointer"="all" "target-cpu"="core2" }
attributes #7 = { noreturn }

!llvm.module.flags = !{!24, !25, !26, !27}
!llvm.dbg.cu = !{!28}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "<std::rt::lang_start::{closure_env#0}<()> as core::ops::function::Fn<()>>::{vtable}", scope: null, file: !2, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "<unknown>", directory: "")
!3 = !DICompositeType(tag: DW_TAG_structure_type, name: "<std::rt::lang_start::{closure_env#0}<()> as core::ops::function::Fn<()>>::{vtable_type}", file: !2, size: 384, align: 64, flags: DIFlagArtificial, elements: !4, vtableHolder: !14, templateParams: !23, identifier: "47832f92273eba5a9b7636610ba21775")
!4 = !{!5, !8, !10, !11, !12, !13}
!5 = !DIDerivedType(tag: DW_TAG_member, name: "drop_in_place", scope: !3, file: !2, baseType: !6, size: 64, align: 64)
!6 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "*const ()", baseType: !7, size: 64, align: 64, dwarfAddressSpace: 0)
!7 = !DIBasicType(name: "()", encoding: DW_ATE_unsigned)
!8 = !DIDerivedType(tag: DW_TAG_member, name: "size", scope: !3, file: !2, baseType: !9, size: 64, align: 64, offset: 64)
!9 = !DIBasicType(name: "usize", size: 64, encoding: DW_ATE_unsigned)
!10 = !DIDerivedType(tag: DW_TAG_member, name: "align", scope: !3, file: !2, baseType: !9, size: 64, align: 64, offset: 128)
!11 = !DIDerivedType(tag: DW_TAG_member, name: "__method3", scope: !3, file: !2, baseType: !6, size: 64, align: 64, offset: 192)
!12 = !DIDerivedType(tag: DW_TAG_member, name: "__method4", scope: !3, file: !2, baseType: !6, size: 64, align: 64, offset: 256)
!13 = !DIDerivedType(tag: DW_TAG_member, name: "__method5", scope: !3, file: !2, baseType: !6, size: 64, align: 64, offset: 320)
!14 = !DICompositeType(tag: DW_TAG_structure_type, name: "{closure_env#0}<()>", scope: !15, file: !2, size: 64, align: 64, elements: !18, templateParams: !23, identifier: "a03113aa7e17c2dfc05379bf1427c674")
!15 = !DINamespace(name: "lang_start", scope: !16)
!16 = !DINamespace(name: "rt", scope: !17)
!17 = !DINamespace(name: "std", scope: null)
!18 = !{!19}
!19 = !DIDerivedType(tag: DW_TAG_member, name: "main", scope: !14, file: !2, baseType: !20, size: 64, align: 64)
!20 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "fn()", baseType: !21, size: 64, align: 64, dwarfAddressSpace: 0)
!21 = !DISubroutineType(types: !22)
!22 = !{null}
!23 = !{}
!24 = !{i32 7, !"PIC Level", i32 2}
!25 = !{i32 7, !"PIE Level", i32 2}
!26 = !{i32 2, !"Dwarf Version", i32 2}
!27 = !{i32 2, !"Debug Info Version", i32 3}
!28 = distinct !DICompileUnit(language: DW_LANG_Rust, file: !29, producer: "clang LLVM (rustc version 1.63.0-nightly (ebbcbfc23 2022-05-27))", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, globals: !30)
!29 = !DIFile(filename: "src/main.rs/@/4n8hqx8i84m93xkw", directory: "/Users/aaroncruz/Desktop/serf/code/aaaa")
!30 = !{!0}
!31 = distinct !DISubprogram(name: "__rust_begin_short_backtrace<fn(), ()>", linkageName: "_ZN3std10sys_common9backtrace28__rust_begin_short_backtrace17h9843a81026573baeE", scope: !33, file: !32, line: 118, type: !35, scopeLine: 118, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !41, retainedNodes: !37)
!32 = !DIFile(filename: "/rustc/ebbcbfc236ced21d5e6a92269edb704692ff26b8/library/std/src/sys_common/backtrace.rs", directory: "", checksumkind: CSK_MD5, checksum: "f7c76d4902bfcea1d96ffad8fbde919d")
!33 = !DINamespace(name: "backtrace", scope: !34)
!34 = !DINamespace(name: "sys_common", scope: !17)
!35 = !DISubroutineType(types: !36)
!36 = !{null, !20}
!37 = !{!38, !39}
!38 = !DILocalVariable(name: "f", arg: 1, scope: !31, file: !32, line: 118, type: !20)
!39 = !DILocalVariable(name: "result", scope: !40, file: !32, line: 122, type: !7, align: 1)
!40 = distinct !DILexicalBlock(scope: !31, file: !32, line: 122, column: 5)
!41 = !{!42, !43}
!42 = !DITemplateTypeParameter(name: "F", type: !20)
!43 = !DITemplateTypeParameter(name: "T", type: !7)
!44 = !DILocation(line: 122, column: 9, scope: !40)
!45 = !DILocation(line: 118, column: 43, scope: !31)
!46 = !DILocation(line: 122, column: 18, scope: !31)
!47 = !DILocation(line: 125, column: 5, scope: !40)
!48 = !DILocation(line: 128, column: 1, scope: !31)
!49 = !DILocation(line: 128, column: 2, scope: !31)
!50 = !DILocation(line: 118, column: 1, scope: !31)
!51 = distinct !DISubprogram(name: "lang_start<()>", linkageName: "_ZN3std2rt10lang_start17h5489c8da79f3ee56E", scope: !16, file: !52, line: 139, type: !53, scopeLine: 139, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !65, retainedNodes: !59)
!52 = !DIFile(filename: "/rustc/ebbcbfc236ced21d5e6a92269edb704692ff26b8/library/std/src/rt.rs", directory: "", checksumkind: CSK_MD5, checksum: "53ba40c54b421907f2e3ab22fb96d5b4")
!53 = !DISubroutineType(types: !54)
!54 = !{!55, !20, !55, !56}
!55 = !DIBasicType(name: "isize", size: 64, encoding: DW_ATE_signed)
!56 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "*const *const u8", baseType: !57, size: 64, align: 64, dwarfAddressSpace: 0)
!57 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "*const u8", baseType: !58, size: 64, align: 64, dwarfAddressSpace: 0)
!58 = !DIBasicType(name: "u8", size: 8, encoding: DW_ATE_unsigned)
!59 = !{!60, !61, !62, !63}
!60 = !DILocalVariable(name: "main", arg: 1, scope: !51, file: !52, line: 140, type: !20)
!61 = !DILocalVariable(name: "argc", arg: 2, scope: !51, file: !52, line: 141, type: !55)
!62 = !DILocalVariable(name: "argv", arg: 3, scope: !51, file: !52, line: 142, type: !56)
!63 = !DILocalVariable(name: "v", scope: !64, file: !52, line: 144, type: !55, align: 8)
!64 = distinct !DILexicalBlock(scope: !51, file: !52, line: 144, column: 5)
!65 = !{!43}
!66 = !DILocation(line: 140, column: 5, scope: !51)
!67 = !DILocation(line: 141, column: 5, scope: !51)
!68 = !DILocation(line: 142, column: 5, scope: !51)
!69 = !DILocation(line: 145, column: 10, scope: !51)
!70 = !DILocation(line: 145, column: 9, scope: !51)
!71 = !DILocation(line: 144, column: 17, scope: !51)
!72 = !DILocation(line: 144, column: 12, scope: !51)
!73 = !DILocation(line: 144, column: 12, scope: !64)
!74 = !DILocation(line: 150, column: 2, scope: !51)
!75 = distinct !DISubprogram(name: "{closure#0}<()>", linkageName: "_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h37f8371ecb54c99dE", scope: !15, file: !52, line: 145, type: !76, scopeLine: 145, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !65, retainedNodes: !80)
!76 = !DISubroutineType(types: !77)
!77 = !{!78, !79}
!78 = !DIBasicType(name: "i32", size: 32, encoding: DW_ATE_signed)
!79 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "&std::rt::lang_start::{closure_env#0}<()>", baseType: !14, size: 64, align: 64, dwarfAddressSpace: 0)
!80 = !{!81}
!81 = !DILocalVariable(name: "main", scope: !75, file: !52, line: 140, type: !20, align: 8)
!82 = !{i64 8}
!83 = !DILocation(line: 140, column: 5, scope: !75)
!84 = !DILocation(line: 145, column: 77, scope: !75)
!85 = !DILocation(line: 145, column: 18, scope: !75)
!86 = !DILocation(line: 145, column: 100, scope: !75)
!87 = distinct !DISubprogram(name: "as_i32", linkageName: "_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h74e27ed49fb0fb0bE", scope: !89, file: !88, line: 485, type: !96, scopeLine: 485, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !23, retainedNodes: !99)
!88 = !DIFile(filename: "/rustc/ebbcbfc236ced21d5e6a92269edb704692ff26b8/library/std/src/sys/unix/process/process_common.rs", directory: "", checksumkind: CSK_MD5, checksum: "0172c472c69d772c784713c132680582")
!89 = !DICompositeType(tag: DW_TAG_structure_type, name: "ExitCode", scope: !90, file: !2, size: 8, align: 8, elements: !94, templateParams: !23, identifier: "84a85bcd6200c621f7ad9c2fe59de0fb")
!90 = !DINamespace(name: "process_common", scope: !91)
!91 = !DINamespace(name: "process", scope: !92)
!92 = !DINamespace(name: "unix", scope: !93)
!93 = !DINamespace(name: "sys", scope: !17)
!94 = !{!95}
!95 = !DIDerivedType(tag: DW_TAG_member, name: "__0", scope: !89, file: !2, baseType: !58, size: 8, align: 8)
!96 = !DISubroutineType(types: !97)
!97 = !{!78, !98}
!98 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "&std::sys::unix::process::process_common::ExitCode", baseType: !89, size: 64, align: 64, dwarfAddressSpace: 0)
!99 = !{!100}
!100 = !DILocalVariable(name: "self", arg: 1, scope: !87, file: !88, line: 485, type: !98)
!101 = !DILocation(line: 485, column: 19, scope: !87)
!102 = !DILocation(line: 486, column: 9, scope: !87)
!103 = !DILocation(line: 487, column: 6, scope: !87)
!104 = distinct !DISubprogram(name: "to_i32", linkageName: "_ZN3std7process8ExitCode6to_i3217h8d7d08954b0dbf9dE", scope: !106, file: !105, line: 1788, type: !110, scopeLine: 1788, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !23, retainedNodes: !112)
!105 = !DIFile(filename: "/rustc/ebbcbfc236ced21d5e6a92269edb704692ff26b8/library/std/src/process.rs", directory: "", checksumkind: CSK_MD5, checksum: "09686d5428418bad4b0f5434bb2554d6")
!106 = !DICompositeType(tag: DW_TAG_structure_type, name: "ExitCode", scope: !107, file: !2, size: 8, align: 8, elements: !108, templateParams: !23, identifier: "62c4090749a57f272904de0501e37d62")
!107 = !DINamespace(name: "process", scope: !17)
!108 = !{!109}
!109 = !DIDerivedType(tag: DW_TAG_member, name: "__0", scope: !106, file: !2, baseType: !89, size: 8, align: 8)
!110 = !DISubroutineType(types: !111)
!111 = !{!78, !106}
!112 = !{!113}
!113 = !DILocalVariable(name: "self", arg: 1, scope: !104, file: !105, line: 1788, type: !106)
!114 = !DILocation(line: 1788, column: 19, scope: !104)
!115 = !DILocation(line: 1789, column: 9, scope: !104)
!116 = !DILocation(line: 1790, column: 6, scope: !104)
!117 = distinct !DISubprogram(name: "call_once<std::rt::lang_start::{closure_env#0}<()>, ()>", linkageName: "_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h0e79ae1531ace3c1E", scope: !119, file: !118, line: 248, type: !123, scopeLine: 248, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !129, retainedNodes: !126)
!118 = !DIFile(filename: "/rustc/ebbcbfc236ced21d5e6a92269edb704692ff26b8/library/core/src/ops/function.rs", directory: "", checksumkind: CSK_MD5, checksum: "3100065230267ed2a3b8753c70d752a6")
!119 = !DINamespace(name: "FnOnce", scope: !120)
!120 = !DINamespace(name: "function", scope: !121)
!121 = !DINamespace(name: "ops", scope: !122)
!122 = !DINamespace(name: "core", scope: null)
!123 = !DISubroutineType(types: !124)
!124 = !{!78, !125}
!125 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "*mut std::rt::lang_start::{closure_env#0}<()>", baseType: !14, size: 64, align: 64, dwarfAddressSpace: 0)
!126 = !{!127, !128}
!127 = !DILocalVariable(arg: 1, scope: !117, file: !118, line: 248, type: !125)
!128 = !DILocalVariable(arg: 2, scope: !117, file: !118, line: 248, type: !7)
!129 = !{!130, !131}
!130 = !DITemplateTypeParameter(name: "Self", type: !14)
!131 = !DITemplateTypeParameter(name: "Args", type: !7)
!132 = !DILocation(line: 248, column: 5, scope: !117)
!133 = distinct !DISubprogram(name: "call_once<std::rt::lang_start::{closure_env#0}<()>, ()>", linkageName: "_ZN4core3ops8function6FnOnce9call_once17h24629aabe3434f74E", scope: !119, file: !118, line: 248, type: !134, scopeLine: 248, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !129, retainedNodes: !136)
!134 = !DISubroutineType(types: !135)
!135 = !{!78, !14}
!136 = !{!137, !138}
!137 = !DILocalVariable(arg: 1, scope: !133, file: !118, line: 248, type: !14)
!138 = !DILocalVariable(arg: 2, scope: !133, file: !118, line: 248, type: !7)
!139 = !DILocation(line: 248, column: 5, scope: !133)
!140 = distinct !DISubprogram(name: "call_once<fn(), ()>", linkageName: "_ZN4core3ops8function6FnOnce9call_once17he75420568aee3710E", scope: !119, file: !118, line: 248, type: !35, scopeLine: 248, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !144, retainedNodes: !141)
!141 = !{!142, !143}
!142 = !DILocalVariable(arg: 1, scope: !140, file: !118, line: 248, type: !20)
!143 = !DILocalVariable(arg: 2, scope: !140, file: !118, line: 248, type: !7)
!144 = !{!145, !131}
!145 = !DITemplateTypeParameter(name: "Self", type: !20)
!146 = !DILocation(line: 248, column: 5, scope: !140)
!147 = distinct !DISubprogram(name: "drop_in_place<std::rt::lang_start::{closure_env#0}<()>>", linkageName: "_ZN4core3ptr85drop_in_place$LT$std..rt..lang_start$LT$$LP$$RP$$GT$..$u7b$$u7b$closure$u7d$$u7d$$GT$17ha14e469f7e4e7619E", scope: !149, file: !148, line: 486, type: !150, scopeLine: 486, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !154, retainedNodes: !152)
!148 = !DIFile(filename: "/rustc/ebbcbfc236ced21d5e6a92269edb704692ff26b8/library/core/src/ptr/mod.rs", directory: "", checksumkind: CSK_MD5, checksum: "82f5019c26bcd7ac80b412794ed2a844")
!149 = !DINamespace(name: "ptr", scope: !122)
!150 = !DISubroutineType(types: !151)
!151 = !{null, !125}
!152 = !{!153}
!153 = !DILocalVariable(arg: 1, scope: !147, file: !148, line: 486, type: !125)
!154 = !{!155}
!155 = !DITemplateTypeParameter(name: "T", type: !14)
!156 = !DILocation(line: 486, column: 1, scope: !147)
!157 = distinct !DISubprogram(name: "black_box<()>", linkageName: "_ZN4core4hint9black_box17hec351bf934961c38E", scope: !159, file: !158, line: 224, type: !160, scopeLine: 224, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !65, retainedNodes: !162)
!158 = !DIFile(filename: "/rustc/ebbcbfc236ced21d5e6a92269edb704692ff26b8/library/core/src/hint.rs", directory: "", checksumkind: CSK_MD5, checksum: "fa192aef52724158854d1e5ce88991e5")
!159 = !DINamespace(name: "hint", scope: !122)
!160 = !DISubroutineType(types: !161)
!161 = !{null, !7}
!162 = !{!163}
!163 = !DILocalVariable(name: "dummy", arg: 1, scope: !157, file: !158, line: 224, type: !7)
!164 = !DILocation(line: 224, column: 27, scope: !157)
!165 = !DILocation(line: 225, column: 5, scope: !157)
!166 = !{i32 3215928}
!167 = !DILocation(line: 226, column: 2, scope: !157)
!168 = distinct !DISubprogram(name: "report", linkageName: "_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17hd12a6c7762e273c4E", scope: !169, file: !105, line: 2138, type: !170, scopeLine: 2138, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !28, templateParams: !23, retainedNodes: !172)
!169 = !DINamespace(name: "{impl#50}", scope: !107)
!170 = !DISubroutineType(types: !171)
!171 = !{!106, !7}
!172 = !{!173}
!173 = !DILocalVariable(name: "self", arg: 1, scope: !168, file: !105, line: 2138, type: !7)
!174 = !DILocation(line: 2138, column: 15, scope: !168)
!175 = !DILocation(line: 2140, column: 6, scope: !168)
!176 = distinct !DISubprogram(name: "main", linkageName: "_ZN4aaaa4main17h3a8ac27ca49c8f71E", scope: !178, file: !177, line: 6, type: !21, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition | DISPFlagMainSubprogram, unit: !28, templateParams: !23, retainedNodes: !179)
!177 = !DIFile(filename: "src/main.rs", directory: "/Users/aaroncruz/Desktop/serf/code/aaaa", checksumkind: CSK_MD5, checksum: "0c7eed08f52511e3bc1fc610c98fffac")
!178 = !DINamespace(name: "aaaa", scope: null)
!179 = !{!180, !186, !188, !190}
!180 = !DILocalVariable(name: "x", scope: !181, file: !177, line: 7, type: !182, align: 4)
!181 = distinct !DILexicalBlock(scope: !176, file: !177, line: 7, column: 5)
!182 = !DICompositeType(tag: DW_TAG_structure_type, name: "Point2D", scope: !178, file: !2, size: 64, align: 32, elements: !183, templateParams: !23, identifier: "3c3055dd31afa100725eab33beda4144")
!183 = !{!184, !185}
!184 = !DIDerivedType(tag: DW_TAG_member, name: "x", scope: !182, file: !2, baseType: !78, size: 32, align: 32)
!185 = !DIDerivedType(tag: DW_TAG_member, name: "y", scope: !182, file: !2, baseType: !78, size: 32, align: 32, offset: 32)
!186 = !DILocalVariable(name: "x_ptr", scope: !187, file: !177, line: 12, type: !9, align: 8)
!187 = distinct !DILexicalBlock(scope: !181, file: !177, line: 12, column: 5)
!188 = !DILocalVariable(name: "y_ptr", scope: !189, file: !177, line: 13, type: !9, align: 8)
!189 = distinct !DILexicalBlock(scope: !187, file: !177, line: 13, column: 5)
!190 = !DILocalVariable(name: "z", scope: !191, file: !177, line: 14, type: !9, align: 8)
!191 = distinct !DILexicalBlock(scope: !189, file: !177, line: 14, column: 5)
!192 = !DILocation(line: 7, column: 9, scope: !181)
!193 = !DILocation(line: 7, column: 13, scope: !176)
!194 = !DILocation(line: 12, column: 53, scope: !181)
!195 = !DILocation(line: 12, column: 33, scope: !181)
!196 = !DILocation(line: 12, column: 9, scope: !187)
!197 = !DILocation(line: 13, column: 53, scope: !187)
!198 = !DILocation(line: 13, column: 33, scope: !187)
!199 = !DILocation(line: 13, column: 9, scope: !189)
!200 = !DILocation(line: 14, column: 13, scope: !189)
!201 = !DILocation(line: 14, column: 9, scope: !191)
!202 = !DILocation(line: 16, column: 2, scope: !176)
