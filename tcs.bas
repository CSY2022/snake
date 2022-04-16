_FullScreen
Screen _NewImage(640, 400, 256)
DECLARE SUB xshz16 (zf$)
Dim Shared ys#
Dim Shared hzk$
Dim Shared er$
Dim w(25, 40)
On Error GoTo erro
er$ = "f"
Open "b", #4, "配置文件误删"
If er$ = "t" Then
    hzk$ = "f"
End If
Do
    ss = 0
    fs = 0
    nn$ = "d"
    endl = 0
    For h = 0 To 24
        For l = 0 To 40
            w(h, l) = 0
        Next l
    Next h
    Color _RGB(0, 127, 0), _RGB(240, 240, 240)
    Cls
    For h = 0 To 24
        For l = 0 To 40
            w(h, l) = 0
        Next l
    Next h
    sl = 0
    sh = 0
    nlo = 1
    lo = 1
    Cls
    Locate 6, 35
    ys# = _RGB(0, 127, 0)
    Call xshz16("贪吃蛇")
    Locate 25, 74
    ys# = _RGB(127, 0, 0)
    Call xshz16("v1.0.1")
    _Title "CSY GAME"
    Locate 19, 29
    ys# = _RGB(0, 0, 127)
    Call xshz16("<按空格键开始游戏>")
    Locate 21, 30
    Call xshz16("<按+键结束游戏>")
    Locate 12, 32
    Call xshz16("最高分：")
    Locate 14, 32
    Call xshz16("总分:")
    Locate 12, 39
    Open "best.dat" For Input As #1
    Input #1, best&
    Close #1
    Print best&
    Open "all.dat" For Input As #1
    Input #1, all&
    Close #1
    Locate 14, 40
    Print all&
    Locate 16, 32
    Call xshz16("得分加成")
    Open "more.dat" For Input As #1
    Input #1, more
    Close #1
    Locate 16, 41
    Print "+"; CInt(more * 10) * 10; "%"
    Do Until iiii = 1
        zf1$ = InKey$
        If zf1$ = " " Then iiii = 1
        If zf1$ = "+" Or zf1$ = "+" Then
            System
        End If
    Loop
    iiii = 0
    timecc = Timer
    sjsz = Sqr((timecc - timec) ^ 3) * 10000
    Randomize sjsz
    For i = 1 To 20
        Randomize Rnd(i)
        Do
            l = Int(Rnd(i) * 400)
            h = Int(Rnd(i) * 250)
        Loop Until l < 40 And h < 25
        w(h, l) = -1
    Next i
    w(0, 0) = 0
    w(0, 1) = 0
    w(1, 0) = 0
    w(1, 1) = 0
    w(0, 2) = 0
    w(1, 2) = 0
    w(2, 2) = 0
    w(2, 0) = 0
    w(0, 1) = 0
    For ii = 0 To 40
        w(0, ii) = 0
    Next ii
    Cls
    If all& < 500 Then
        Locate 10, 36
        ys# = _RGB(0, 127, 0)
        Call xshz16("规则")
        Locate 13, 6
        Call xshz16("用方向键控制绿色蛇，它开始只有一格长度，每吃一次食物就增加一格长度")
        Locate 15, 6
        Call xshz16("其中蓝色为食物，红色为障碍，碰到后游戏结束。按空格键继续吧！")
        Locate 21, 30
        Call xshz16("<按+键结束游戏>")
        Locate 12, 32
        Do Until iiii = 1
            zf1$ = InKey$
            If zf1$ = " " Then iiii = 1
            If zf1$ = "+" Or zf1$ = "+" Then
                System
            End If
        Loop
        iiii = 0
    End If
    Cls
    Do
        pk$ = InKey$
        ss = 0
        Select Case pk$
            Case "+"
                System
            Case "W"
                nn$ = "w"
            Case "A"
                nn$ = "a"
            Case "S"
                nn$ = "s"
            Case "D"
                nn$ = "d"
            Case Chr$(0) + Chr$(72)
                nn$ = "w"
            Case Chr$(0) + Chr$(75)
                nn$ = "a"
            Case Chr$(0) + Chr$(80)
                nn$ = "s"
            Case Chr$(0) + Chr$(77)
                nn$ = "d"
            Case "w"
                nn$ = "w"
            Case "a"
                nn$ = "a"
            Case "s"
                nn$ = "s"
            Case "d"
                nn$ = "d"
            Case Else
                If nn$ = "" Then
                    Locate 21, 34
                    Call xshz16("请按WASD键操作")
                End If
        End Select
        Color , _RGB(240, 240, 240)
        For h = 0 To 24
            For l = 0 To 40
                If w(h, l) = -1 Then Line (l * 16 , h * 16 )-Step(15, 15), _RGB(127, 0, 0), BF
                If w(h, l) = -2 Then Line (l * 16, h * 16)-Step(15, 15), _RGB(0, 0, 127), BF: ss = ss + 1
                If w(h, l) > nlo - lo Then Line (l * 16, h * 16)-Step(15, 15), _RGB(0, 127, 0), BF
                If w(h, l) = nlo - lo Then Line (l * 16, h * 16)-Step(15, 15), _RGB(240, 240, 240), BF
                If w(h, l) = nlo Then Line (l * 16, h * 16)-Step(15, 15), _RGB(0, 64, 0), BF
            Next l
        Next h
        If Timer - timec132 > 10 Then
            Cls
            timec132 = Timer
        End If
        If Timer - timec12 > 0.1 - (0.001 * lo) Then
            Select Case nn$
                Case "w"
                    sh = sh - 1: nlo = nlo + 1
                Case "a"
                    sl = sl - 1: nlo = nlo + 1
                Case "s"
                    sh = sh + 1: nlo = nlo + 1
                Case "d"
                    sl = sl + 1: nlo = nlo + 1
            End Select
            timec12 = Timer
        End If
        Do
            l = Int(Rnd(i) * 400)
            h = Int(Rnd(i) * 250)
        Loop Until l < 40 And h < 25
        If w(h, l) >= 0 And ss < 10 Then
            If w(h, l) < nlo - lo Then
                If Timer - timeccc > 5 Then
                    w(h, l) = -2
                    timeccc = Timer
                End If
            End If
        End If
        Locate 1, 1
        Call xshz16("得分")
        Locate 1, 6
        Print fs
        Locate 21, 30
        Call xshz16("<按+键结束游戏>")
        If w(sh, sl) >= 0 Then w(sh, sl) = nlo
        If w(sh, sl) = -1 Then endl = 1
        If w(sh, sl) = -2 Then w(sh, sl) = nlo: fs = CInt(fs + 100 + 100 * more): lo = lo + 1: ss = ss - 1
        If sl < 0 Or sh < 0 Then endl = 1: fs = CInt(fs - 100 - 100 * more)
        If sl > 39 Or sh > 24 Then endl = 1
    Loop While endl = 0
    Cls
    Locate 12, 35
    Color _RGB(127, 0, 0)
    Print "GAME OVER"
    Locate 14, 36
    Call xshz16("得分")
    Locate 14, 42
    Print fs
    If fs > best& Then
        Open "best.dat" For Output As #1
        Print #1, fs
        Close #1
    End If
    Open "all.dat" For Input As #1
    Input #1, all&
    Close #1
    Open "all.dat" For Output As #1
    Print #1, fs + all&
    Close #1
    Open "more.dat" For Input As #1
    Input #1, more
    Close #1
    Open "more.dat" For Output As #1
    Print #1, CInt((more + 0.0001 * fs) * 100) / 100
    Close #1
    timec = Timer
    Do Until Timer - timec > 1.5
    Loop
Loop
q: System
erro:
errs = Err
er$ = "t"
Resume Next
DefInt A-Z
Sub xshz16 (zf$)
    If ys# = 0 Then ys# = _RGB(0, 0, 0)
    Color ys#, _RGB(240, 240, 240)
    If hzk$ = "f" Then
        Print zf$;
        Exit Sub
    End If
    hzbz = CsrLin
    zzbz = Pos(hzb)
    zzb = hzbz * 16 - 15
    hzb = zzbz * 8 - 7
    hzbtmp = hzb
    l = Len(zf$)
    i = 1
    Do While i <= l
        zf1 = Asc(Mid$(zf$, i, 1)): i = i + 1
        If zf1 < 159 Then
            Locate hzbz, zzbz
            Color ys#
            Print Chr$(zf1);
            hzb = hzb + 8
            zzbz = zzbz + 1
        Else
            zf2 = Asc(Mid$(zf$, i, 1)): i = i + 1
            Seek #4, ((zf1 - 161) * 94& + zf2 - 161) * 32 + 1
            s32$ = Input$(32, # 4)
            i2 = 1
            For a = 0 To 15
                s = CVI(Mid$(s32$, i2 + 1, 1) + Mid$(s32$, i2, 1))
                Line (hzb - 1, zzb + a - 1)-Step(15, 0), ys#, , s
                i2 = i2 + 2
            Next a
            hzb = hzb + 16
            zzbz = zzbz + 2
        End If
    Loop
    ys# = 0
End Sub

