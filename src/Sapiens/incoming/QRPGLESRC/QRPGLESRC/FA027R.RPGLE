     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA027R - WRITE FMFNP AND FMF1P RECORD.                   S DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA027R                                            *
     H*                                                               *
     H*    CREATED: 12/31/97  C:62                                    *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*    C:180 09/01/17  Project: 114MHV                            *
     H*                    Copy field CXCVT#                          *
     H*                                                               *
     H*    C:309 11/10/16  Project: 90MIB                             *
     H*                    Add logic to map WMFNP/FNCLELIG to         *
     H*                    FMFNP/FNCLELIG                             *
     H*                                                               *
     H*    C:58  08/25/00  Proj: WC993 Update FNTEST if maintenance
     H*                                as well as new record.
     H*    C:58  06/28/00  Proj: WC993 Update new field FNTEST.
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     F*
     F*** BEG CHG *** 01/20/99 ***********************************
     F*FMFNP * O * E *****              DISK                      A
     F********************                          KINFDS DBFDS
     F*FMF1P * O * E *****              DISK                      A
     FFMFNL1    UF A E           K DISK
     F                                     INFDS(DBFDS)
     FFMF1L     UF A E           K DISK
     F*** END CHG *** 01/20/99 ***********************************
     FWMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     F*** BEG CHG *** 09/01/17 ***********************************
     FWMCXL     IF   E           K DISK
     FFMCXL     UF A E           K DISK    Rename(PCX01:FCX01) Prefix(o_)
     F*** END CHG *** 09/01/17 ***********************************

     D*** END ADD ***** 06/28/00 *************************************
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPFN02
     I              FNCO#                       FXCO#
     I              FNFND                       FXFND
     I              FNNAME                      FXNAME
     I              FNSHNM                      FXSHNM
     I              FNMAD1                      FXMAD1
     I              FNMAD2                      FXMAD2
     I              FNMCTY                      FXMCTY
     I              FNMST                       FXMST
     I              FNMZIP                      FXMZIP
     I              FNPAD1                      FXPAD1
     I              FNPAD2                      FXPAD2
     I              FNPCTY                      FXPCTY
     I              FNPST                       FXPST
     I              FNPZIP                      FXPZIP
     I              FNPH1A                      FXPH1A
     I              FNPH1                       FXPH1
     I              FNAREA                      FXAREA
     I              FNPH2                       FXPH2
     I              FNFAXA                      FXFAXA
     I              FNFAX                       FXFAX
     I              FNEFDT                      FXEFDT
     I              FNEXDT                      FXEXDT
     I              FNSFIN                      FXSFIN
     I              FNCGFL                      FXCGFL
     I              FNMXEX                      FXMXEX
     I              FNNACS                      FXNACS
     I              FNNAEN                      FXNAEN
     I              FNNAQN                      FXNAQN
     I              FNAUTE                      FXAUTE
     I              FNAUTD                      FXAUTD
     I              FNDIV0                      FXDIV0
     I              FNRS01                      FXRS01
     I              FNRS02                      FXRS02
     I              FNRS03                      FXRS03
     I              FNRS04                      FXRS04
     I              FNRS05                      FXRS05
     I              FNRS06                      FXRS06
     I              FNRS07                      FXRS07
     I              FNRS08                      FXRS08
     I              FNRS09                      FXRS09
     I              FNRS10                      FXRS10
     I              FNDC01                      FXDC01
     I              FNDC02                      FXDC02
     I              FNDC03                      FXDC03
     I              FNDC04                      FXDC04
     I              FNDC05                      FXDC05
     I              FNDC06                      FXDC06
     I              FNDC07                      FXDC07
     I              FNDC08                      FXDC08
     I              FNDC09                      FXDC09
     I              FNDC10                      FXDC10
     I              FNMETR                      FXMETR
     I              FNMEP1                      FXMEP1
     I              FNCOTR                      FXCOTR
     I              FNEXTR                      FXEXTR
     I              FNEXP1                      FXEXP1
     I              FNLETR                      FXLETR
     I              FNLEP1                      FXLEP1
     I              FNDFRC                      FXDFRC
     I              FNDFRA                      FXDFRA
     I              FNCHK1                      FXCHK1
     I              FNCHK2                      FXCHK2
     I              FNLHNF                      FXLHNF
     I              FNLHF                       FXLHF
     I              FNANAM                      FXANAM
     I              FNAAD1                      FXAAD1
     I              FNAAD2                      FXAAD2
     I              FNACTY                      FXACTY
     I              FNAST                       FXAST
     I              FNAZIP                      FXAZIP
     I              FNINVK                      FXINVK
     I              FNSRPR                      FXSRPR
     I              FNLNRB                      FXLNRB
     I              FNLNAB                      FXLNAB
     I              FNLBX1                      FXLBX1
     I              FNLBX2                      FXLBX2
     I              FNLBX3                      FXLBX3
     I              FNLBX4                      FXLBX4
     I              FNCLMS                      FXCLMS
     I              FNNOT1                      FXNOT1
     I              FNNOT2                      FXNOT2
     I              FNSTAT                      FXSTAT
     I              FNUSER                      FXUSER
     I              FNPGM                       FXPGM
     I              FNTIME                      FXTIME
     I              FNDT                        FXDT
     I              FNIUSR                      FXIUSR
     I              FNIPGM                      FXIPGM
     I              FNITIM                      FXITIM
     I              FNIDT                       FXIDT
     I              FNIADR                      FXIADR
     I              FNMIDY                      FXMIDY
     I*** BEGIN ADD *** 06/28/00 *************************************
     I              FNTEST                      FXTEST
    I*** Beg Add ***** 11/10/16 *************************************
     I              FNCLELIG                    FXCLELIG
    I*** End Add ***** 11/10/16 *************************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@GRP                          WCMC GROUP #
     C                   PARM                    @@FND                          WCMS GROUP #
     C                   PARM                    @@RRN             9 0          WCMC GROUP RRN
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    FNCO#         @@CO#
     C     *LIKE         DEFINE    FNSUB#        @@SUB#
     C     *LIKE         DEFINE    FNFND         @@GRP
     C     *LIKE         DEFINE    FNFND         @@FND
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    FNDT          XXDT                           ADD 01/20/99
     C     *LIKE         DEFINE    FNSUB#        KKSUB#                         ADD 01/20/99
     C*
     C* KEY FOR GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*** BEG ADD *** 01/20/99 ***********************************
     C*
     C* KEY FOR GROUP AUX FILE
     C*
     C     FNKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C** INIT
     C                   CLEAR                   PF101
     C                   CLEAR                   PFN01
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C*
     C     FNKY          CHAIN     PFN01                              61
     C*
     C     *IN61         IFEQ      '0'
     C     @@SUB#        ANDEQ     *ZEROS
     C                   Z-ADD     FNSUB#        @@SUB#
     C                   ENDIF
     C*
     C                   MOVE      @@SUB#        KKSUB#
     C*** END ADD *** 01/20/99 ***********************************
     C*** ADD RECORD
     C*  MASTER RECORD
     C*
     C*
     C* LOAD KEY VALUES FOR FMFNP.
     C*
     C*
     C* GET WC GROUP INFORMATION AND LOAD THAT INFORMATION INTO MC GRP.
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     FNKY          CHAIN     PFN02                              99
     C*
     C*** ON ADD POPULATE FIELDS
     C*
     C     *IN61         IFEQ      *ON                                          ADD 01/20/99
     C                   MOVE      @@CO#         FNCO#
     C                   MOVE      @@SUB#        FNSUB#
     C                   MOVE      @@GRP         FNFND
     C                   MOVE      @@FND         FNWCF#
     C*
     C                   MOVEL     FXNAME        FNNAME
     C                   MOVEL     FXSHNM        FNSHNM
     C                   MOVEL     FXMAD1        FNMAD1
     C                   MOVEL     FXMAD2        FNMAD2
     C                   MOVEL     FXMCTY        FNMCTY
     C                   MOVEL     FXMST         FNMST
     C                   MOVE      FXMZIP        FNMZIP
     C                   MOVEL     FXPAD1        FNPAD1
     C                   MOVEL     FXPAD2        FNPAD2
     C                   MOVEL     FXPCTY        FNPCTY
     C                   MOVEL     FXPST         FNPST
     C                   MOVE      FXPZIP        FNPZIP
     C                   MOVE      FXPH1A        FNARA1
     C                   MOVE      FXPH1         FNPH1
     C                   MOVE      FXAREA        FNAREA
     C                   MOVE      FXPH2         FNPH2
     C                   MOVE      FXFAXA        FNFARA
     C                   MOVE      FXFAX         FNFAX
     C                   Z-ADD     FXEXDT        FNEXDT
     C                   MOVEL     'A'           FNSTAT
     C*** BEGIN ADD *** 06/28/00 *************************************
     C                   MOVEL     FXTEST        FNTEST
     C*** END ADD ***** 06/28/00 *************************************
    C*** Beg Add ***** 11/10/16 *************************************
     C                   MOVEL     FXCLELIG      FNCLELIG
    C*** End Add ***** 11/10/16 *************************************
     C*
     C* LOAD MAINTENANCE AND INIT FIELDS FOR FMFNP
     C*
     C                   Z-ADD     DSTIME        FNITIM
     C                   MOVEL     Q1USER        FNIUSR
     C                   MOVE      XXDT          FNIDT
     C*
     C*** BEG ADD *** 01/20/99 ***********************************
     C*
     C*** ON UPDATE ONLY UPDATE SPECIFIC FIELDS
     C*
     C                   ELSE
     C*** BEGIN ADD *** 08/25/00 *************************************
     C                   MOVEL     FXTEST        FNTEST
     C*** END ADD ***** 08/25/00 *************************************
    C*** Beg Add ***** 11/10/16 *************************************
     C                   MOVEL     FXCLELIG      FNCLELIG
    C*** End Add ***** 11/10/16 *************************************
     C                   Z-ADD     FXEXDT        FNEXDT
     C                   ENDIF
     C*** END ADD *** 01/20/99 ***********************************
     C*
     C                   Z-ADD     DSTIME        FNTIME
     C                   MOVEL     XXDT          FNDT
     C                   MOVEL     Q1PGM         FNPGM
     C                   MOVEL     Q1USER        FNUSER
     C*
     C     *IN61         IFEQ      *ON                                          ADD 01/20/99
     C                   WRITE     PFN01
     C                   ELSE                                                   ADD 01/20/99
     C                   UPDATE    PFN01                                        ADD 01/20/99
     C                   ENDIF                                                  ADD 01/20/99
     C*  DETAIL RECORD
     C*
     C     FNKY1         CHAIN     PF101                              98        ADD 01/20/99
     C*
     C                   MOVEL     XXDT          F1DT
     C                   MOVEL     Q1PGM         F1PGM
     C                   MOVEL     Q1USER        F1USER
     C***********          TIME           DSDTTM           DEL 01/20/99
     C                   Z-ADD     DSTIME        F1TIME
     C*
     C     *IN98         IFEQ      *ON                                          ADD 01/20/99
     C                   MOVE      @@CO#         F1CO#
     C                   MOVE      @@SUB#        F1SUB#
     C                   MOVE      @@GRP         F1FND
     C                   MOVEL     'N'           F1OUTA
     C                   Z-ADD     DSTIME        F1ITIM
     C                   MOVEL     Q1USER        F1IUSR
     C                   MOVE      XXDT          F1IDT
     C                   MOVEL     'A'           F1STAT
     C                   WRITE     PF101
     C                   ELSE                                                   ADD 01/20/99
     C*
     C                   UPDATE    PF101                                        ADD 01/20/99
     C                   ENDIF                                                  ADD 01/20/99
     C*
     C                   MOVE      DBRRN         @@RRN
     C*
      /Free
       //***** Begin Add *** 09/01/17 *****************************************/
       Chain (KKco#: KKfnd) WMCXL;
       If %found(WMCXL);
          Chain (KKco#:KKsub#:KKfnd) FMCXL;
          If %found(FMCXL);
             o_CXCVT# = CXCVT#;
             Update  FCX01;
          Endif;
       Endif;
       //***** End   Add *** 09/01/17 *****************************************/
      /End-Free
     C*  END THE PROGRAM
     C                   RETURN
