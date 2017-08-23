c---- STF: Subset Table Files
c   vsn 1.3:  added -r and -d options; -r # causes a randomly selected
c             fraction # of data rows to be passed; -d # causes every
c             #th line to be deleted (not passed through)
c   vsn 1.4:  added -D # to pass only each #th line
c   vsn 1.5:  lengthened FilNam to 1000 characters
c   vsn 1.51: added "-r", "-d", "-D" to tutorial; expanded to 5000
c             fields, 500 command-line arguments
c
                            Integer MaxFld, MaxArg
                       Parameter (MaxFld = 5000, MaxArg = 500)
c
      Character*9999 Line
      Character*1000 TmpFld,FilNam
      Character*100  FldNam(MaxFld),ArgNam(MaxArg),Field
      Character*25   NumStr
      Character*10   Fmt
      Logical        Keep(MaxFld),List,GotPipe,DoHed1,DoHed2,DoHed3,
     +               Used(MaxArg), Del1N, Kep1N, PassRand
      Real*4         RandPass
      Integer*4      Col1(MaxFld),IArgC,NArgs,Access,I,J,J1,J2,K,N,
     +               NFlds,LNBlnk,N2, Nout, Nread, N1Del, N1Keep
c
      Data Keep/MaxFld*.False./, List/.False./, GotPipe/.False./,
     +     DoHed1/.True./, DoHed2/.True./, DoHed3/.True./, N2/0/,
     +     Used/MaxArg*.False./, NFlds/0/, PassRand,Del1N/2*.false./,
     +     Nout/0/, Nread/0/, Kep1N/.false./
c
c-----------------------------------------------------------------------
c
      NArgs = IArgC()
c
      If (NArgs .gt. MaxArg) Then
        Write (0, 7000) NArgs, MaxArg
        NArgs = MaxArg
      End If
c
      If (NArgs .lt. 2) Then
        Write (6, 6000)
        Stop
      End If
c
      Call GetArg(1, FilNam)
      If (Access(FilNam,' ') .ne. 0) Then
        Write (0,7001) FilNam
        Stop
      End If
c
      I = 0
5     I = I + 1
      if (I .le. NArgs-1) then
        Call GetArg(I+1, ArgNam(I))
        If ((ArgNam(I) .eq. '-l') .or. (ArgNam(I) .eq. '-L')) Then
          List = .True.
          Used(I) = .True.
        Else If ((ArgNam(I) .eq. '-s1') .or. (ArgNam(I) .eq. '-S1'))
     +  Then
          DoHed1 = .False.
          Used(I) = .True.
        Else If ((ArgNam(I) .eq. '-s2') .or. (ArgNam(I) .eq. '-S2'))
     +  Then
          DoHed2 = .False.
          Used(I) = .True.
        Else If ((ArgNam(I) .eq. '-s3') .or. (ArgNam(I) .eq. '-S3'))
     +  Then
          DoHed3 = .False.
          Used(I) = .True.
        Else If ((ArgNam(I) .eq. '-s') .or. (ArgNam(I) .eq. '-S')) Then
          DoHed1 = .False.
          DoHed2 = .False.
          DoHed3 = .False.
          Used(I) = .True.
        Else If (ArgNam(I) .eq. '-d') Then
          Used(I) = .True.
          I = I + 1
          call GetArg(I+1, NumStr)
          read (NumStr, *, err=3000) N1Del
          Used(I) = .True.
          Del1N = .true.
        Else If (ArgNam(I) .eq. '-D') Then
          Used(I) = .True.
          I = I + 1
          call GetArg(I+1, NumStr)
          read (NumStr, *, err=3002) N1Keep
          Used(I) = .True.
          Kep1N = .true.
        Else If ((ArgNam(I) .eq. '-r') .or. (ArgNam(I) .eq. '-R')) Then
          Used(I) = .True.
          I = I + 1
          call GetArg(I+1, NumStr)
          read (NumStr, *, err=3001) RandPass
          Used(I) = .True.
          PassRand = .true.
        Else If ((ArgNam(I)(1:1) .ge. '0') .and.
     +           (ArgNam(I)(1:1) .le. '9')) Then
          Used(I) = .True.
          K = Index(ArgNam(I),'-')
          If (K .gt. 0) Then
            Field = ArgNam(I)(1:K-1)
            Call GetFmt(Field,Fmt)
            Read(Field,Fmt) J1
            Field = ArgNam(I)(K+1:LNBlnk(ArgNam(I)))
            Call GetFmt(Field,Fmt)
            Read(Field,Fmt) J2
            If (J1 .gt. J2) Then
              J  = J1
              J1 = J2
              J2 = J
            End If
            Call Check(J1)
            Call Check(J2)
            Do 10 J = J1, J2
              Keep(J) = .True.
10          Continue
          Else
            Call GetFmt(ArgNam(I),Fmt)
            Read(ArgNam(I),Fmt) J
            Call Check(J)
            Keep(J) = .True.
          End If
        Else
          N2 = N2 + 1
        End If
        go to 5
      end if
c
      NArgs = NArgs - 1
      N = 0
      Do 30 I = 1, MaxFld
        If (Keep(I)) N = N + 1
30    Continue
c
      If ((N .eq. 0) .and. (N2 .eq. 0) .and. (.not.List)) Then
        Write (0, 7002)
        Stop
      End If
c
      Do 40 I = 1, NArgs
        Call UpCase(ArgNam(I))
40    Continue
c
      Open (60, File = FilNam)
      Read (60, 6060, End = 1000) Line
      If (Line(1:1) .eq. '|') GotPipe = .True.
      If ((.not.GotPipe) .and. (DoHed1)) Then
        Call MakeFmt(Fmt,LNBlnk(Line))
        Write (6, Fmt) Line
        Write (6, 6006)
      End If
c
60    If (.not.GotPipe) Go to 100
      K = 0
      J = LNBlnk(Line)
      Do 90 I = 1, J
        If (Line(I:I) .eq. '|') Then
          If (K .eq. MaxFld-1) Then
            Write (0,7003) MaxFld
            Go to 200
          End If
          K = K + 1
          Col1(K) = I
          If (K .gt. 1) Then
            TmpFld = Line(Col1(K-1)+1:I-1)
            FldNam(K-1) = TmpFld
70          If ((LNBlnk(TmpFld) .gt. 1) .and.
     +                 (TmpFld(1:1) .eq. ' ')) Then
              Do 80 J1 = 2, LNBlnk(TmpFld)
                TmpFld(J1-1:J1-1) = TmpFld(J1:J1)
80            Continue
              J1 = LNBlnk(TmpFld)
              TmpFld(J1:J1) = ' '
              FldNam(K-1) = TmpFld
              Go to 70
            End If
          End If
        End If
90    Continue
c
      NFlds = K - 1
      Do 94 J = 1, NFlds
        Field = FldNam(J)
        Call UpCase(Field)
        Do 92 I = 1, NArgs
          If (Field .eq. ArgNam(I)) Then
            If (.not.Keep(J)) N = N + 1
            Keep(J) = .True.
            Used(I) = .True.
          End If
92      Continue
94    Continue
c
      If ((N .eq. 0) .and. (.not.List)) Then
        Write (0, 7002)
        Go to 1000
      End If
c
      If (DoHed1 .and. DoHed3 .and. (N .gt. 0)) Then
        Write (6, 6001)
        Call MakeFmt(Fmt,LNBlnk(FilNam))
        Write (6, Fmt) FilNam
        Write (6, 6006)
        Write (6, 6010)
        Do 96 I = 1, MaxFld
          If (Keep(I)) Then
            If (I .lt. 10) Then
               Write (6, 6002) I
            Else If (I .lt. 100) Then
               Write (6, 6003) I
            Else If (I .lt. 1000) Then
               Write (6, 6004) I
            Else
               Write (6, 6005) I
            End If
          End If
96      Continue
        Write (6, 6006)
      End If
c
      Go to 200
c
100   Read (60, 6060, End = 1000) Line
      If (Line(1:1) .eq. '|') GotPipe = .True.
      If ((.not.GotPipe) .and. (DoHed1)) Then
        Call MakeFmt(Fmt,LNBlnk(Line))
        Write (6, Fmt) Line
        Write (6, 6006)
      End If
      Go to 60
c
200   If (.not.List) Go to 300
      Write (6, 6007)
      Do 220 I = 1, NFlds
        Field = FldNam(I)(1:LNBlnk(FldNam(I)))
        Call MakeFmt(Fmt,LNBlnk(Field))
        Write (6, 6008) I
        Write (6, Fmt) Field
        Write (6, 6006)
220   Continue
      If (N .eq. 0) Go to 1000
c
300   If ((DoHed2) .or. (Line(1:1) .ne. '|')) Then
        if (Line(1:1) .ne. '|') then
          Nread = Nread + 1
          if (PassRand) then
            if (Rand(0) .gt. RandPass) go to 500
          else if (Del1N) then
            if (mod(Nread,N1Del) .eq. 0) go to 500
          else if (Kep1N) then
            if (mod(Nread,N1Keep) .ne. 0) go to 500
          end if
        end if
        Do 400 I = 1, NFlds
          If (Keep(I)) Then
            J2 = Col1(I+1) - 1
            Field = Line(Col1(I):J2)
            J1 = J2 - Col1(I) + 1
            Call MakeFmt(Fmt,J1)
            Write (6, Fmt) Field
          End If
400     Continue
        If (Line(1:1) .eq. '|') Then
          Write (6, 6009)
        Else
          Write (6, 6006)
        End If
        Nout = Nout + 1
      End If
c
500   Read (60, 6060, End = 1000) Line
      Go to 300
c
1000  N = 0
      Do 1010 I = 1, NArgs
        If (.not.Used(I)) N = N + 1
1010  Continue
c
      If (N .gt. 0) Then
        Write (0, 7004)
        If (N .eq. 1) Then
          Write (0, 7005)
        Else
          Write (0, 7006)
        End If
        Do 1020 I = 1, NArgs
          If (.not.Used(I)) Then
            Call MakeFmt(Fmt,LNBlnk(ArgNam(I))+1)
            Write (0, Fmt) ArgNam(I)
          End If
1020    Continue
        Write (0, 6006)
        Write (0, 6006)
      End If
c
      Stop
c
3000  print *,'Bad value for "-d":',NumStr
      stop
c
3001  print *,'Bad value for "-r":',NumStr
      stop
c
3002  print *,'Bad value for "-D":',NumStr
      stop
c
c-----------------------------------------------------------------------
c%%%WinMac: b 26
6000  Format('STF 1.51 B40430  (Subset Table Files)'/
     + /'usage:  stf <filename> [options]'//
     + 'where [options] may be:'/
     + '       -l    (list field names by number)'/
     + '       ##    (field number to output)'/
     + '    ##-##    (field number range)'/
     + '    <name>   (field name)'/
     + '      -s1    (strip "\" header lines)'/
     + '      -s2    (strip "|" header lines)'/
     + '      -s3    (don''t add STF header lines)'/
     + '       -s    (strip all header lines)'/
     + '       -r #  (pass randomly selected fraction # of data rows)'/
     + '       -d #  (skip every #th line, don''t pass through)'/
     + '       -D #  (pass only each #th line)'//
     + /'example:  stf s123.prpts 1-5 7 psfmag psfsg'//
     + 'fields indicated will be echoed to stdout in the order in which'
     +/'they occur in the input file; header fields will be copied'/
     + 'unchanged, with two STF header lines added, unless turned off;'/
     + 'limits: 9999 characters/line, 100 characters/field'/
     + '        5000 fields/line,     500 arguments')
6001  Format('\  Subset generated by STF 1.51 from file '$)
6002  Format(I2$)
6003  Format(I3$)
6004  Format(I4$)
6005  Format(I5$)
6006  Format()
6007  Format('Field Names:')
6008  Format(I5,': '$)
6009  Format('|')
6010  Format('\  Fields retained by STF: '$)
6060  Format(A9999)
7000  Format('Too many arguments:',I6,'; keeping',I4)
7001  Format('File not found: ',A100)
7002  Format('No fields selected for output')
7003  Format(/'WARNING: too many fields in file; clipped to',I5)
7004  Format(/'WARNING: the following argument'$)
7005  Format(' was not used: '$)
7006  Format('s were not used: '$)
c
c-----------------------------------------------------------------------
c
      End
c
c=======================================================================
c
      Subroutine UpCase(Field)
c
      Character*100 Field
      Character*1   TmpChar
      Integer*4     I,K,LNBlnk
      Byte          TmpByte
      Equivalence (TmpByte,TmpChar)
c
      K = LNBlnk(Field)
      Do 10 I = 1, K
        TmpChar = Field(I:I)
        If ((TmpByte .gt. 96) .and. (TmpByte .lt. 123)) Then
          TmpByte = TmpByte - 32
          Field(I:I) = TmpChar
        End If
10    Continue
c
      Return
c
      End
c
c=======================================================================
c
      Subroutine GetFmt(Field,Fmt)
      Character*100 Field
      Character*10  Fmt
      Integer*4     K,LNBlnk
c
      K = LNBlnk(Field)
      Fmt = ' '
      If (K .lt. 10) Then
        Write(Fmt(3:3),'(I1)') K
        Fmt(4:4) = ')'
      Else If (K .lt. 100) Then
        Write(Fmt(3:4),'(I2)') K
        Fmt(5:5) = ')'
      Else If (K .lt. 1000) Then
        Write(Fmt(3:5),'(I3)') K
        Fmt(6:6) = ')'
      Else
        Write(Fmt(3:6),'(I4)') K
        Fmt(7:7) = ')'
      End If
c
      Fmt(1:2) = '(I'
c
      Return
c
      End
c
c=======================================================================
c
      Subroutine Check(J)
c
                            Integer MaxFld
                       Parameter (MaxFld = 1000)
      Integer*4 J
      Logical   Warnd
c
      Data Warnd/.False./
c
      If (J .lt. 1) Then
        If (.not.Warnd) Write (0,7000) J
        J = 1
        Warnd = .True.
      End If
c
      If (J .gt. MaxFld) Then
        If (.not.Warnd) Write (0,7000) J
        J = MaxFld
        Warnd = .True.
      End If
c
      Return
c
7000  Format('WARNING: out-of-limits field number rejected:',I9/
     + '         further bad values will be rejected but not flagged')
c
      End
c
c=======================================================================
c
      Subroutine MakeFmt(Fmt,K)
      Character*10   Fmt
      Integer        K
c
      Fmt = ' '
      If (K .lt. 10) Then
        Write(Fmt(3:3),'(I1)') K
        Fmt(4:5) = '$)'
      Else If (K .lt. 100) Then
        Write(Fmt(3:4),'(I2)') K
        Fmt(5:6) = '$)'
      Else If (K .lt. 1000) Then
        Write(Fmt(3:5),'(I3)') K
        Fmt(6:7) = '$)'
      Else
        Write(Fmt(3:6),'(I4)') K
        Fmt(7:8) = '$)'
      End If
c
      Fmt(1:2) = '(A'
c
      Return
c
      End
