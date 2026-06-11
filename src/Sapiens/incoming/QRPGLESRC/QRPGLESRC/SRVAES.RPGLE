     H**********************************************************************
     H*(C) Copyright FISERV Insurance Solutions, Inc., 2000 - 2006
    H**********************************************************************
    H*PROGRAM: SRVAES
    H*DESCRIPTION: API - CHECK FOR VALID ENDORSEMENT STATE
     H**********************************************************************
     H*CREATED: 07/17/07 C: 184 PROJECT: WC1529
     H**********************************************************************
     H**********************************************************************
     F*
     Fswcpf     if   e             disk
     Fststl4    if   e           k disk
     F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D*
     Dnccd             s              2    dim(52) ctdata perrcd(1)
     Dkkcd             s                   like(stnst)
     D@@form           s             15
     D@@rtn            s              1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     C     *entry        plist
     C                   parm                    @@form
     C                   parm                    @@rtn
     C*
     C     stky          klist
     C                   kfld                    kkcd
     C*
     C*=====================================================================
     C*MAIN PROGRAM
     C*=====================================================================
     C*
     C                   Eval      @@rtn = 'Y'
     C*Check the overall endorsement authorization
     C                   Read      swcpf
     C*
     C                   If        not %eof and wcfl34 = 'Y'
     C*
     C                   Eval      kkcd = %subst(@@form:3:2)
     C*
     C                   if        %lookup(kkcd:nccd) <> 0
     C*
     C     stky          chain     pst01
     C                   if        %found  and
     C                             stenau <> 'Y'
     C*
     C                   Eval      @@rtn = 'N'
     C*
     C                   Endif
     C                   Endif
     C                   Endif
     C*
     C                   Eval      *inlr=*on
    C*====================================================================*
**  NCCD - VALID NCCI STATE CODES
01
02
03
04
05
06
07
08
09
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
52
54
