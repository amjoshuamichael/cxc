source_filename = "new_module"
;target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-Fi8-f64:32:64-v64:32:64-v128:32:128-i32:64:64-a:0:32-n32-S32"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-darwin21.5.0"
;target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
;target triple = "x86_64-unknown-linux-gnu"
;define i32 @main() {
;entry:
;  %new_point = alloca <{ i32, i32 }>, align 4
;  %access = getelementptr inbounds <{ i32, i32 }>, <{ i32, i32 }>* %new_point, i32 0, i32 0
;  %x_ptr = alloca i32*, align 8
;  store i32* %access, i32** %x_ptr, align 8
;  %access1 = getelementptr inbounds <{ i32, i32 }>, <{ i32, i32 }>* %new_point, i32 0, i32 1
;  %y_ptr = alloca i32*, align 8
;  store i32* %access1, i32** %y_ptr, align 8
;  %t0 = load i32*, i32** %y_ptr, align 8
;  %t1 = load i32*, i32** %x_ptr, align 8
;  %i0 = ptrtoint i32* %t0 to i32
;  %i1 = ptrtoint i32* %t1 to i32
;  %sub = sub i32 %i0, %i1
;  ret i32 0
;}

;define i32 @main() {
;  %x = alloca { i32, i32 }, align 4
;  %t0 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %x, i32 0, i32 0
;  store i32 128, i32* %t0, align 8
;  %t1 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %x, i32 0, i32 1
;  store i32 128, i32* %t1, align 8
;  ret i32 1000 
;}
define <{ i32, i32 }> @main() {
entry:
  %new_point = alloca <{ i32, i32 }>, align 4
  store <{ i32, i32 }> zeroinitializer, <{ i32, i32 }>* %new_point, align 1
  %access = getelementptr inbounds <{ i32, i32 }>, <{ i32, i32 }>* %new_point, i32 0, i32 0
  store i32 128, i32* %access, align 8
  %access1 = getelementptr inbounds <{ i32, i32 }>, <{ i32, i32 }>* %new_point, i32 0, i32 1
  store i32 128, i32* %access1, align 8
  %t0 = load <{ i32, i32 }>, <{ i32, i32 }>* %new_point, align 1
  ret <{ i32, i32 }> %t0
}

