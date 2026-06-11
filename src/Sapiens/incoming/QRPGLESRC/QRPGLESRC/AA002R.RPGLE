     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  AA001R - API FOR MASS UPDATE OF FAEAP          C:94 12/30/98 *
     H*                                                               *
     H*  CALLED FROM: N0215R                                          *
     H*                                                               *
     H*  CALLS TO   : SA001R - API THAT ASSIGNS VARIOUS SYSTEM FLAGS. *
     H*             SRCVTDAT - DATE CONVERSION PROGRAM.               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/03/99  C:94  PROJ: MC152                                  *
     H*                  DO NOT WRITE OR UPDATE RECON#.  THIS SHOULD  *
     H*                  ONLY BE DONE IN BILL ENTRY.                  *
     H*                                                               *
     H*****************************************************************
     H*  NOTE:                                                        *
     H*  @@WRUP IS A 7 POSITION ALPHA FIELD.  THE VALID OPTIONS ARE A *
     H*  'W' IF IT IS TO WRITE TO A FILE AND A 'U' IF IT IS TO UPDATE *
     H*  A FILE.  FAEOP, FAE1P, NAMBP, AND FAEAP CAN ALSO BE A 'D'    *
     H*  FOR A DELETE.                                                *
     H*  THE FOLLOWING ARE WHAT FILE EACH POSITION IS FOR:            *
     H*      POSITION 1:  FAEOP                                       *
     H*      POSITION 2:  FAE1P                                       *
     H*      POSITION 3:  FAHBP                                       *
     H*      POSITION 4:  NAPBP                                       *
     H*      POSITION 5:  NAMBP                                       *
     H*      POSITION 6:  NATRP                                       *
     H*      POSITION 7:  FAEAP                                       *
     H*  @@WRUP CAN THEREFORE LOOK LIKE THIS:  WU U D                 *
     H*                                                               *
     H*****************************************************************
     F*
     FNATRL     UF   E           K DISK
     F*
     FFAHBL     UF   E           K DISK
     F*
     FNAPBL1    UF   E           K DISK
     F*
     FFAEAL1    UF   E           K DISK
     F                                     INFDS(DBFDS)
     F*
     FNAMBL     UF   E           K DISK
     F                                     INFDS(DBFDS3)
     F*
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRNB               397    400B 0
     D DBFDS3          DS
     D  DBRRNC               397    400B 0
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7

     I*

     C*
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EASUB#        KKSUB#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EACASE        KKCASE
     C     *LIKE         DEFINE    EASS#         KKSS#
     C     *LIKE         DEFINE    EAACDT        KKACDT
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0          COMPANY #
     C                   PARM                    @@SUB#            5 0          SUBSCRIBER #
     C                   PARM                    @@EOB#            9 0          EOB #
     C                   PARM                    @@RCN             3 0          RECON #
     C                   PARM                    @@PGM            10            NAME OF PGM CALLING
     C                   PARM                    @@BAMT            9 2          BRS AMOUNT
     C                   PARM                    @@NAMT            9 2          NWS AMOUNT
     C                   PARM                    @@BHRF            1            BRS INV H/R FLAG
     C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
     C                   PARM                    @@BINV            9 0          BRS INVOICE #
     C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
     C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
     C                   PARM                    @@NINV            9 0          NWS INVOICE #
     C                   PARM                    @@SYS             1            TYPE OF SERVICE
     C                   PARM                    @@WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    @@TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    @@TMAA            9 2          TIME/MAT AFTER
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     HBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     MBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     PBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     TRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     E1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C*----------------------------------------------------------------
     C*  START OF PROGRAM
     C*----------------------------------------------------------------
     C*
     C                   CLEAR                   PEA01
     C                   CLEAR                   PMB01
     C                   CLEAR                   PPB01
     C                   CLEAR                   PTR01
     C                   CLEAR                   PHB01
     C*
     C                   MOVEL     @@WRUP        KKWRUP
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C                   PARM                    @@RHAB            1            REHAB INST
     C                   PARM                    @@MSS             1            MS SUPPORT
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILL
     C*
     C                   EXSR      MAIN
     C*
     C     XXFAE1        IFEQ      'D'
     C     XXNAMB        OREQ      'D'
     C     XXFAEA        OREQ      'D'
     C                   EXSR      DELUPD
     C                   ENDIF
     C*
     C     @@PRVB        IFEQ      'Y'
     C                   EXSR      UPFAEA
     C                   ELSE
     C                   EXSR      UPREST
     C                   ENDIF
     C*
     C                   RETURN
     C*
     C*---------------------------------------------------------------
     C* MAIN SUBROUTINE - GET THE MAIN INFORMATION FROM FMCMP
     C*---------------------------------------------------------------
     C*
     C     MAIN          BEGSR
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#
     C*
     C                   ENDSR
     C*
     C*---------------------------------------------------------------
     C* UPFAEA SUBROUTINE - UPDATE FAEAP IF PROVIDER BILL CONTROL FLAG
     C*                     EQUALS 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPFAEA        BEGSR
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     @@EOB#        IFNE      *ZEROS
     C                   Z-ADD     @@EOB#        EAEOB#
     C                   ENDIF
     C*
     C*** BEG DEL *** 03/03/99 **************************************
     C********** @@RCN     IFNE *ZEROS
     C**********           Z-ADD@@RCN     EARCN
     C**********           ENDIF
     C*** END DEL *** 03/03/99 **************************************
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        EABAMT
     C                   ENDIF
     C*
     C     @@NAMT        IFNE      *ZEROS
     C                   Z-ADD     @@NAMT        EANAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        EABHRF
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        EABBLF
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        EABINV
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        EABCAM
     C                   ENDIF
     C*
     C     @@NHRF        IFNE      *BLANK
     C                   MOVEL     @@NHRF        EANHRF
     C                   ENDIF
     C*
     C     @@NBLF        IFNE      *BLANK
     C                   MOVEL     @@NBLF        EANBLF
     C                   ENDIF
     C*
     C     @@NINV        IFNE      *ZEROS
     C                   Z-ADD     @@NINV        EANINV
     C                   ENDIF
     C*
     C     @@SYS         IFNE      *BLANK
     C                   MOVEL     @@SYS         EASYS
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     @@PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   UPDATE    PEA01
     C*
     C     EAKY          READE     PEA01                                  99
     C                   ENDDO
     C*
     C                   ENDSR                                                  UPFAEA
     C*
     C*---------------------------------------------------------------
     C* UPREST SUBROUTINE - UPDATE FAHBP, NAPBP, NATRP, NAMBP, AND
     C*                     FAEOP IF PROVIDER BILL CONTROL FLAG NE 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPREST        BEGSR
     C*
     C                   SELECT
     C*
     C     @@SYS         WHENEQ    'H'
     C                   EXSR      UPFAHB
     C*
     C     @@SYS         WHENEQ    'U'
     C                   EXSR      UPNAPB
     C*
     C     @@SYS         WHENEQ    'M'
     C                   EXSR      UPNAMB
     C*
     C     @@SYS         WHENEQ    'N'
     C                   EXSR      UPNATR
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  UPFAEA
     C*
     C*---------------------------------------------------------------
     C* UPFAHB SUBROUTINE - UPDATE FAHBP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPFAHB        BEGSR
     C*
     C*
     C*---------------------\
     C*  UPDATE FAHBP       |
     C*---------------------/
     C*
     C     XXFAHB        IFEQ      'U'
     C*
     C     HBKY          CHAIN     PHB01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        HB1AAM
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        HBHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        HBBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        HBINV#
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        HB1CAM
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        HBUSER
     C                   MOVEL     @@PGM         HBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBDT
     C*
     C                   MOVEL     DSTIME        HBTIME
     C*
     C                   UPDATE    PHB01
     C*
     C     HBKY          READE     PHB01                                  99
     C                   ENDDO                                                  *IN99
     C*
     C                   ENDIF                                                  XXFAHB = U
     C*
     C                   ENDSR                                                  UPFAHB
     C*
     C*---------------------------------------------------------------
     C* UPNAPB SUBROUTINE - UPDATE NAPBP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPNAPB        BEGSR
     C*
     C*---------------------\
     C*  UPDATE NAPBP       |
     C*---------------------/
     C*
     C     XXNAPB        IFEQ      'U'
     C*
     C     PBKY          CHAIN     PPB01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        PBBAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        PBHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        PBBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        PBINV#
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        PBCAMT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        PBUSER
     C                   MOVEL     @@PGM         PBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PBDT
     C*
     C                   MOVEL     DSTIME        PBTIME
     C*
     C                   UPDATE    PPB01
     C*
     C     PBKY          READE     PPB01                                  99
     C                   ENDDO                                                  *IN99
     C*
     C                   ENDIF                                                  XXNAPB = U
     C*
     C                   ENDSR                                                  UPNAPB
     C*
     C*---------------------------------------------------------------
     C* UPNAMB SUBROUTINE - UPDATE NAMBP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPNAMB        BEGSR
     C*
     C*---------------------\
     C*  UPDATE NAMBP       |
     C*---------------------/
     C*
     C     XXNAMB        IFEQ      'U'
     C*
     C     MBKY          CHAIN     PMB01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     @@BAMT        IFNE      *ZEROS
     C                   Z-ADD     @@BAMT        MBBAMT
     C                   ENDIF
     C*
     C     @@BHRF        IFNE      *BLANK
     C                   MOVEL     @@BHRF        MBHRFL
     C                   ENDIF
     C*
     C     @@BBLF        IFNE      *BLANK
     C                   MOVEL     @@BBLF        MBBLFL
     C                   ENDIF
     C*
     C     @@BINV        IFNE      *ZEROS
     C                   Z-ADD     @@BINV        MBINV#
     C                   ENDIF
     C*
     C     @@BCAM        IFNE      *ZEROS
     C                   Z-ADD     @@BCAM        MBCAMT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        MBUSER
     C                   MOVEL     @@PGM         MBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBDT
     C*
     C                   MOVEL     DSTIME        MBTIME
     C*
     C                   UPDATE    PMB01
     C*
     C     MBKY          READE     PMB01                                  99
     C                   ENDDO                                                  *IN99
     C*
     C                   ENDIF                                                  XXNAMB = U
     C*
     C                   ENDSR                                                  UPNAMB
     C*
     C*---------------------------------------------------------------
     C* UPNATR SUBROUTINE - UPDATE NATRP IF PROVIDER BILL CONTROL FLAG
     C*                     DOES NOT EQUAL 'Y'
     C*---------------------------------------------------------------
     C*
     C     UPNATR        BEGSR
     C*
     C*---------------------\
     C*  UPDATE NATRP       |
     C*---------------------/
     C*
     C     XXNATR        IFEQ      'U'
     C*
     C     TRKY          CHAIN     PTR01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     @@NAMT        IFNE      *ZEROS
     C                   Z-ADD     @@NAMT        TRBAMT
     C                   ENDIF
     C*
     C     @@NHRF        IFNE      *BLANK
     C                   MOVEL     @@NHRF        TRHRFL
     C                   ENDIF
     C*
     C     @@NBLF        IFNE      *BLANK
     C                   MOVEL     @@NBLF        TRBLFL
     C                   ENDIF
     C*
     C     @@NINV        IFNE      *ZEROS
     C                   Z-ADD     @@NINV        TRINV#
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        TRUSER
     C                   MOVEL     @@PGM         TRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRDT
     C*
     C                   MOVEL     DSTIME        TRTIME
     C*
     C                   UPDATE    PTR01
     C*
     C     TRKY          READE     PTR01                                  99
     C                   ENDDO                                                  *IN99
     C*
     C                   ENDIF                                                  XXNATR = U
     C*
     C                   ENDSR                                                  UPNATR
     C*
     C*================================================================
     C* DELEB1 - Deletes record from EOMB auxiliary file FAE1P.
     C*================================================================
     C     DELEB1        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DELMBT - Deletes record from Medical Bill Trans. File NAMBP.
     C*================================================================
     C     DELMBT        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DELFAE - Deletes record from Bill Header Amount  File FAEAP.
     C*================================================================
     C     DELFAE        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* DELUPD - Deletes records from FAEAP, FAE1P and NAMBP.
     C*================================================================
     C     DELUPD        BEGSR
     C*
     C* Deletes record from EOMB auxiliary file (FAE1OP).
     C*
     C                   EXSR      DELEB1
     C*
     C* Deletes record from Medical Mgmt Bill transfer file (NAMBP).
     C*
     C                   EXSR      DELMBT
     C*
     C* Deletes record from Bill Header Amount File (FAEAP).
     C*
     C                   EXSR      DELFAE
     C*
     C                   ENDSR
