     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  09/26/17 C:33 Project: 146WGV                                *
     H*                If runninng AO conversion, use co# from WMFNP, *
     H*                 if only one exists.                           *
     H*                                                               *
     H*  05/10/99 C:33 SEQ#: WC051/17998                              *
     H*                ADD DIVISION RESTRICTION PARM.                 *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK

     d***  Begin Add  ***  09/26/17  *************************************
     dGetUserCo#       pr             3  0
     d***  End   Add  ***  09/26/17  *************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@CONM
     C                   PARM                    @@FNAM
     C                   PARM                    @@LVL
     C                   PARM                    @@TYPE
     C                   PARM                    @@OUTQ
     C                   PARM                    @@XCO#
     C                   PARM                    @@XFND
     C                   PARM                    @@XEMP
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C                   PARM                    @@XDIV
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C     COKEY         KLIST                                                  COMPANY
     C                   KFLD                    UPCO#
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C     UPKEY         KLIST                                                  USER PROFILE
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPCO#         @@CO#
     C     *LIKE         DEFINE    UPFND         @@FND
     C     *LIKE         DEFINE    CONAME        @@CONM
     C     *LIKE         DEFINE    CONAME        @@FNAM
     C     *LIKE         DEFINE    UPLVL         @@LVL
     C     *LIKE         DEFINE    UPTYPE        @@TYPE
     C     *LIKE         DEFINE    UPOUTQ        @@OUTQ
     C     *LIKE         DEFINE    UPXCO#        @@XCO#
     C     *LIKE         DEFINE    UPXFND        @@XFND
     C     *LIKE         DEFINE    UPXEMP        @@XEMP
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     *LIKE         DEFINE    UPFL5         @@XDIV
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C                   MOVE      *ZEROS        UPCO#
     C                   MOVE      *ZEROS        UPFND
     C                   MOVE      *BLANKS       FNNAME
     C                   MOVE      *BLANKS       @@CONM
     C                   MOVE      *BLANKS       @@FNAM
     C*
     C***  Begin Chg  ***  09/26/17  *************************************
     C*****UPKEY         CHAIN     PUP01                              99
B01  C******IN99         IFEQ      *OFF
      /Free
       If GetUserCo# <> *zero;
      /End-free
     C***  End   Chg  ***  09/26/17  *************************************
     C     COKEY         CHAIN     PCO01                              99
B02  C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        @@CONM
     C     FNKEY         CHAIN     PFN01                              99
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   MOVE      UPCO#         @@CO#
     C                   MOVE      UPFND         @@FND
     C                   MOVEL     FNNAME        @@FNAM
     C*
B01  C     Q1PRMS        IFGT      4
     C                   MOVE      UPLVL         @@LVL
E01  C                   ENDIF                                                  Q1PRMS>4
     C*
B01  C     Q1PRMS        IFGT      5
     C                   MOVE      UPTYPE        @@TYPE
E01  C                   ENDIF                                                  Q1PRMS>5
      *
B01  C     Q1PRMS        IFGT      6
     C                   MOVE      UPOUTQ        @@OUTQ
E01  C                   ENDIF                                                  Q1PRMS IFGT 6
      *
B01  C     Q1PRMS        IFGT      7
     C                   MOVE      UPXCO#        @@XCO#
E01  C                   ENDIF                                                  Q1PRMS>7
      *
B01  C     Q1PRMS        IFGT      8
     C                   MOVE      UPXFND        @@XFND
E01  C                   ENDIF                                                  Q1PRMS>8
      *
B01  C     Q1PRMS        IFGT      9
     C                   MOVE      UPXEMP        @@XEMP
E01  C                   ENDIF                                                  Q1PRMS>9
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     Q1PRMS        IFGT      10
     C                   MOVE      UPFL5         @@XDIV
     C                   ENDIF                                                  Q1PRMS>10
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C                   RETURN
     P***  Begin Add  ***  09/26/17  *************************************
     P*===================================================================
     PGetUserCo#       b
     dGetUserCo#       pi             3  0

     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  inFound                       1

     d decCo#          s              3  0 inz
     d isFound         s              1n   inz
     d isSingleCo#     s              1n   inz('1')
      /Free
       Clear UPco#;
       Chain UPkey pup01;
       If UPco# <> *zero;
          Return UPco#;
       Endif;
       Exsr CheckSingleCo#;
       ChkStack('SYAOCVTC':isFound);
       If isFound and isSingleCo#;
          UPco# = decCo#;
       Endif;
       Return UPco#;

       //-----------------------------------------------------------------
       Begsr CheckSingleCo#;
        Setll *loval pfn01;
        Dou %eof;
           Read pfn01;
           Select;
           When %eof;
              Iter;
           When FNco# = decCo#;
              Iter;
           When decCo# <> *zero;
              isSingleCo# = *off;
           Endsl;
           decCo# = FNco#;
        Enddo;
       Endsr;
      /End-free
     PGetUserCo#       e
     P***  End   Add  ***  09/26/17  *************************************
