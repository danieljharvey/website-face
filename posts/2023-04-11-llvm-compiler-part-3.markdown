


```llvm
; ModuleID = 'example'





declare external ccc  void @printint(i32)


define external ccc  i32 @sum(i32  %a_0, i32  %b_0)    {
  %1 = add   i32 %a_0, %b_0
  ret i32 %1
}


define external ccc  i32 @main()    {
  %1 =  call ccc  i32  @sum(i32  20, i32  22)
   call ccc  void  @printint(i32  %1)
  ret i32 0
}
```
