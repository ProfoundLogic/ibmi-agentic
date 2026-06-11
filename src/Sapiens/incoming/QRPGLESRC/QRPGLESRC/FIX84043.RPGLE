     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H**********************************************************************
     H* Created:
     H* 10/11/19 C: 079 Support: 84043
     H**********************************************************************
     F*
     Ffau1p     if   e             Disk
     Ffau2l2    if   e           k Disk
     Ffddep     o    e             Disk
     Ffddel     uf   e           k Disk    Rename(pde01:pde02)
     d*=====================================================================
     d* Standalone fields
     d*=====================================================================
     d kkSeq#          s              5s 0
     d hdSeq#          s              5s 0
     d xxEOB           s                   Like(DEeob)
     d xxEoDa          s                   Like(DEnot1)
     d xxEoDb          s                   Like(DEnot1)
     d n1              s              3s 0
     d p1              s              3s 0
     d p2              s              3s 0
     d xxBlk           s              1a
     d xxPos1          s              1a
     d sys_date        s               D   datfmt(*USA)
     d sys_time        s               T
     d*=====================================================================
     d* Data structures
     d*=====================================================================
     d pdds          esds                  extname(SZQ1P)
     d*=====================================================================
     d* Copybooks
     d*=====================================================================
     d/copy sprDate
     C*
     c*=====================================================================
     c* Key lists
     c*=====================================================================
     C     DEKY1         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     C                   KFLD                    U2DSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     C                   KFLD                    U2DSEQ
     C                   KFLD                    KKSEQ#
      /Free
       Read pu101;
       Dow not %Eof(fau1p);
          If U1prFl = 'Y' and U1prDt >= 1190701 and U1prDt <= 1191011;
             Setll (U1co#:U1sub#:U1fnd:U1fYr:U1case:U1eob#) pde02;
             Reade (U1co#:U1sub#:U1fnd:U1fYr:U1case:U1eob#) pde02;
             Dow not %Eof(fddel);
                If DEPgm = 'F8042R';
                   Delete pde02;
                Endif;
                Reade (U1co#:U1sub#:U1fnd:U1fYr:U1case:U1eob#) pde02;
             Enddo;
             ExSr LodEOBCdNotes;
          Endif;
          Read pu101;
       Enddo;
       *InLr = *on;
       Return;
      /End-free
       //====================================================
       // WrtCdNote - Writes EOB Codes and Notes to FDDEP
       //====================================================
     C     WrtCdNote     Begsr
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY2         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             HDSEQ#
     C                   ELSE
     C                   Z-ADD     1             HDSEQ#
     C                   ENDIF
     C*
     C* This code checks for EOB Codes/Notes in the expanded fields
     C*
     C                   SETOFF                                       71
     C     U2EOB1        IFNE      *BLANKS
     C     U2EOD1        ORNE      *BLANKS
     C                   MOVEL     U2eob1        xxEob
      /Free
                         If %Subst(U2eob1:4:1) <> ' ' and
                           %Subst(U2eob1:2:1) <> ' ';
                            xxEob = %Subst(U2eob1:2:3);
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD1:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD1:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD1:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C*
     C                   ENDIF
     C*
     C     U2EOB2        IFNE      *BLANKS
     C     U2EOD2        ORNE      *BLANKS
     C                   MOVEL     U2eob2        xxEob
      /Free
                         If %Subst(U2eob2:4:1) <> ' ' and
                           %Subst(U2eob2:2:1) <> ' ';
                            xxEob = %Subst(U2eob2:2:3);
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD2:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD2:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD2:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C*
     C                   ENDIF
     C*
     C     U2EOB3        IFNE      *BLANKS
     C     U2EOD3        ORNE      *BLANKS
     C                   MOVEL     U2eob3        xxEob
      /Free
                         If %Subst(U2eob3:4:1) <> ' ' and
                           %Subst(U2eob3:2:1) <> ' ';
                            xxEob = %Subst(U2eob3:2:3);
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD3:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD3:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD3:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C                   ENDIF
     C*
     C     U2EOB4        IFNE      *BLANKS
     C     U2EOD4        ORNE      *BLANKS
     C                   MOVEL     U2eob4        xxEob
      /Free
                         If %Subst(U2eob4:4:1) <> ' ' and
                           %Subst(U2eob4:2:1) <> ' ';
                            xxEob = %Subst(U2eob4:2:3);
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD4:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD4:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD4:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C                   ENDIF

     C                   Endsr
     C*================================================================
     C* WRTDE1 - Write FDDEP record
     C*================================================================
     C     WRTDE1        BEGSR
      /Free
          sys_date = %date();
          sys_time = %time();
      /End-Free
     C*
     C                   CLEAR                   PDE01
     C                   Z-ADD     U1CO#         DECO#
     C                   Z-ADD     U1SUB#        DESUB#
     C                   Z-ADD     U1FND         DEFND
     C                   Z-ADD     U1FYR         DEFYR
     C                   Z-ADD     U1CASE        DECASE
     C                   MOVEL     U1SS#         DESS#
     C                   Z-ADD     U1ACDT        DEACDT
     C                   Z-ADD     U1EOB#        DEEOB#
     C                   Z-ADD     U2DSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   ADD       1             HDSEQ#
     C*
     C                   MOVEL(P)  XXEOB         DEEOB
     C                   MOVEL(P)  XXEODA        DENOT1
     C                   MOVEL(P)  XXEODB        DENOT2
     C     1             SUBST     DENOT2:1      XXPOS1            1
     C     XXPOS1        IFEQ      *BLANK
     C     69            SUBST(P)  DENOT2:2      DENOT2
     C                   ENDIF
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  Q1USER        DEUSER
     C                   MOVEL(P)  Q1PGM         DEPGM
      /Free
                         DEdt   = Dateto7(sys_date);
                         DEtime = TimeTo6(SystemTime());
                         DEiDt  = Dateto7(sys_date);
                         DEiTim = TimeTo6(SystemTime());
      /End-Free
     C                   MOVEL(P)  Q1USER        DEIUSR
     C                   MOVEL(P)  Q1PGM         DEIPGM
     C                   MOVEL(P)  'U'           DEESRC
     C*
     C     U2rLin        Ifne      0
     C                   Z-ADD     1             DERCN
     C                   Else
     C                   Z-Add     0             DErcn
     C                   Endif
     C*
     C                   MOVEL(P)  'Y'           DEPRNT
     C*
     C                   WRITE     PDE01
     C*
     C                   ENDSR
       //====================================================
       // LodEOBCdNotes - Loads EOB Codes and Notes into FDDEP
       //====================================================
     C     LodEOBCdNotes Begsr
      /Free
         Setll U1eob# pu201;
         Reade U1eob#  pu201;
         Dow Not %Eof(fau2l2);
         // Write EOB Codes and Notes
            ExSr WrtCdNote;
            Reade U1eob# pu201;
         Enddo;
      /End-Free
     C                   Endsr
