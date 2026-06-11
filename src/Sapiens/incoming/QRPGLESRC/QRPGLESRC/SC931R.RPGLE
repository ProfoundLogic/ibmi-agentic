     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.              **
     H*                                                            **
     H* PROGRAM:     SC931R                                        **
     H* DESCRIPTION: PURGE ROUTINE - DELETE SYSTEM FILES           **
     H*                                                            **
     H* CREATED BY:  WENDY DILLARD        DATE:  03/31/92          **
     H*            BASED ON FEE SCHEDULE PURGE FILE:               **
     H*              FPRG3R                                        **
     H***************************************************************
     H* REVISIONS:                                                 **
     H*                                                            **
     H*   10/17/11 C:279 Project: SY195                            **
     H*                  - Modify program to pass/return UNC path  **
     H*                    field and IFS flag to called/calling    **
     H*                    programs.                               **
     H*                                                            **
     H* 07/28/93  C:10  PROJ: ATD                                  **
     H*                 ADDED PROCESSING FOR PURGE OF INACTIVE     **
     H*                 REMARKS TO THIS PROGRAM.                   **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  50              GENERAL CHAIN/DOWEQ                       **
     H*  51              GENERAL CHAIN/DOWEQ                       **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     H* FILES:                                                     **
     H* PRIMARY FILE -                                             **
     H* SDCMLE:  REMARK DETAIL FILE (KEY/SEQ#/DT)                  **
     H* PDCMP:  SAVE FILE                                          **
     H*                                                            **
     H* SMARL1:  ADDITIONAL REMARKS FILE  (KEY/SEQ#/ASQ#)          **
     H* PMARP:  SAVE FILE                                          **
     H***************************************************************
     FSDCMLE    UF   E           K DISK
     FSMARL1    UF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FPMARP     O    E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FSDCMLH    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FSMDLL     IF   E           K DISK
     FSXERP     O    E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  ADDITIONAL REMARKS FILE
     C*
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ARKY          KLIST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXSEQ#
     C                   KFLD                    XXASQ#
     C*
     C*  ADDITIONAL REMARKS FILE
     C*
     C     ARKY1         KLIST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXSEQ#
     C*** BEGIN ADD *** 07/28/93 **************************************
     C*
     C*  ADDITIONAL REMARKS PURGE FILE
     C*
     C     ARKYP         KLIST
     C                   KFLD                    ARKEY
     C                   KFLD                    ARSEQ#
     C                   KFLD                    ARASQ#
     C*
     C*  REMARK DETAIL FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    ARKEY
     C                   KFLD                    ARSEQ#
     C*
     C*  DOCUMENTATION LOCATION FILE
     C*
     C     DLKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C*  REMARK TYPE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
     C*
     C     *LIKE         DEFINE    CMRMCD        XXRMCD
     C     *LIKE         DEFINE    ARASQ#        XXASQ#
     C     *LIKE         DEFINE    CMKEY         XXKEY
     C     *LIKE         DEFINE    CMSEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMDCID        XXDCID                         ADD 07/28/93
     C     *LIKE         DEFINE    CMFOLD        XXFOLD                         ADD 07/28/93
     C     *LIKE         DEFINE    CMFL01        XXFL01                         ADD 07/28/93
     C     *LIKE         DEFINE    CMUNCN        XXUNCN                         ADD 10/17/11
     C     *LIKE         DEFINE    CMIFSP        XXIFSP                         ADD 10/17/11
     C*
     C                   TIME                    DSDTTM
     C*
     C*   READ REMARK DETAIL RECORD
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*** BEGIN ADD *** 07/28/93 **************************************
     C                   MOVE      *BLANKS       XXDCID
     C                   MOVE      *BLANKS       XXFOLD
     C                   MOVE      *BLANKS       XXFL01
     C*
     C*  IF REMARK HAS A DOCUMENT ID ATTACHED, CHECK IF IT IS ON
     C*     OPTICAL.  IF IT IS, DON'T DELETE THE DOCUMENT OR THE
     C*     REMARK.
     C*
     C     CMDCID        IFNE      *BLANKS
     C     CMFL01        ANDNE     'Y'
     C*
     C     DLKY          CHAIN     PDL01                              62
     C     *IN62         IFEQ      '0'
     C                   GOTO      SKIP
     C                   END
     C*
     C                   END
     C*
     C     CMSTAT        IFEQ      'I'
     C*
     C*   PROCESS ADDITIONAL REMARKS RECORDS
     C*
     C     ARKY1         CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C     ARDT          IFLT      CMDT
     C                   DELETE    PAR01
     C                   END
     C*
     C     ARKY1         READE     PAR01                                  50
     C                   END                                                    DOWEQ PAR01
     C*
     C*** BEGIN ADD *** 07/28/93 **************************************
     C                   MOVEL     CMDCID        XXDCID
     C                   MOVEL     CMFOLD        XXFOLD
     C                   MOVEL     CMFL01        XXFL01
     C*** END   ADD *** 07/28/93 **************************************
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     CMUNCN        XXUNCN
     C                   MOVEL     CMIFSP        XXIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C*   PROCESS REMARK DETAIL RECORD
     C*
     C                   DELETE    PCM01
     C*** BEGIN ADD *** 07/28/93 **************************************
     C*
     C* CHECK TO SEE IF THIS DOCUMENT IS ATTACHED TO ANOTHER REMARK.
     C*    IF IT IS, DON'T DELETE IT.
     C*
     C* MAKE SURE THIS IS NOT A DATA ENTRY PROGRAM (CMFL01 = 'Y')
     C*
     C     XXDCID        IFNE      *BLANKS
     C     XXFL01        ANDNE     'Y'
     C*
     C     XXDCID        CHAIN     PCM02                              63
     C     *IN63         IFEQ      '1'
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'DLDLOC'
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD            2
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    XXUNCN
     C                   PARM                    XXIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C* IF ERROR CODE IS RETURNED FROM DELETE DOCUMENT PGM,
     C*    WRITE ERROR TO WORKFILE (LIST OF ERRORS IS IN DOCUMENTATION)
     C*
     C     QQRTCD        IFNE      *BLANKS
     C                   MOVEL     QQRTCD        ERNUM
     C                   MOVEL     XXDCID        ERDCID
     C                   MOVEL     XXFOLD        ERFOLD
     C                   MOVEL     Q1USER        ERUSER
     C                   MOVEL     Q1PGM         ERPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERDT
     C*
     C                   Z-ADD     DSTIME        ERTIME
     C                   WRITE     PER01
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   ELSE                                                   STAT <> 'I'
     C*** END   ADD *** 07/28/93 **************************************
     C*
     C     CMFL02        IFEQ      'Y'
     C     CMDCDT        IFNE      *ZERO
     C                   MOVE      CMRMCD        XXRMCD
     C     CTKY          CHAIN     PCT01                              61
     C     *IN61         IFEQ      '0'
     C     CTDPRG        IFGT      *ZERO
     C*
     C*  CALCULATE # OF DAYS SINCE DIARY COMPLETION
     C*
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFROM            8 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXTO              8 0
     C*
     C                   MOVE      XXFROM        QQFROM            8 0
     C                   MOVE      XXTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C*  IF # DAYS LEFT ON SYSTEM IS <= # DAYS SINCE DIARY COMPLETION,
     C*     VALUE IS PURGED.
     C*
     C     CTDPRG        IFLE      QQDAYS
     C*
     C                   MOVE      CMKEY         XXKEY
     C                   MOVE      CMSEQ#        XXSEQ#
     C                   MOVE      *LOVAL        XXASQ#
     C*
     C*   PROCESS ADDITIONAL REMARKS RECORDS
     C*
     C     ARKY          SETLL     PAR01
     C     ARKY1         CHAIN     PAR01                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PAR01                                        PURGE FILE
     C*
     C     ARKY1         READE     PAR01                                  50
     C                   END                                                    DOWEQ PAR01
     C*
     C*** BEGIN ADD *** 07/28/93 **************************************
     C                   MOVEL     CMDCID        XXDCID
     C                   MOVEL     CMFOLD        XXFOLD
     C                   MOVEL     CMFL01        XXFL01
     C*** END   ADD *** 07/28/93 **************************************
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   MOVEL     CMUNCN        XXUNCN
     C                   MOVEL     CMIFSP        XXIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C*   PROCESS REMARK DETAIL RECORD
     C*
     C                   DELETE    PCM01                                        PURGE FILE
     C*
     C*** BEGIN ADD *** 07/28/93 **************************************
     C*
     C* CHECK TO SEE IF THIS DOCUMENT IS ATTACHED TO ANOTHER REMARK.
     C*    IF IT IS, DON'T DELETE IT.
     C*
     C* NEED TO CHECK TO MAKE SURE THIS IS NOT A DATA ENTRY PROGRAM
     C*
     C     XXDCID        IFNE      *BLANKS
     C     XXFL01        ANDNE     'Y'
     C*
     C     XXDCID        CHAIN     PCM02                              63
     C     *IN63         IFEQ      '1'
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'DLDLOC'
     C                   PARM                    XXDCID
     C                   PARM                    XXFOLD
     C                   PARM                    QQRTCD            2
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   PARM                    XXUNCN
     C                   PARM                    XXIFSP
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C* IF ERROR CODE IS RETURNED FROM DELETE DOCUMENT PGM,
     C*    WRITE ERROR TO WORKFILE (LIST OF ERRORS IS IN DOCUMENTATION)
     C*
     C     QQRTCD        IFNE      *BLANKS
     C                   MOVEL     QQRTCD        ERNUM
     C                   MOVEL     XXDCID        ERDCID
     C                   MOVEL     XXFOLD        ERFOLD
     C                   MOVEL     Q1USER        ERUSER
     C                   MOVEL     Q1PGM         ERPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERDT
     C*
     C                   Z-ADD     DSTIME        ERTIME
     C                   WRITE     PER01
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*** END   ADD *** 07/28/93 **************************************
     C*
     C                   END                                                    CTDPRG LE QQDAYS
     C*
     C                   END                                                    #DAYS GT 0
     C*
     C                   END                                                    *IN61 EQ 0
     C*
     C                   END                                                    DCDT NE 0
     C*
     C                   END                                                    FL02 EQ Y
     C*
     C*** BEGIN ADD *** 07/28/93 **************************************
     C                   END                                                    END STAT = I
     C*
     C     SKIP          TAG
     C*** END   ADD *** 07/28/93 **************************************
     C*
     C*** READ NEXT RECORD
     C                   READ      PCM01                                  60
     C                   END                                                    ENDDOW *IN60
     C*
     C*** BEGIN ADD *** 07/28/93 **************************************
     C*
     C*  DELETE ANY REMAINING ADDITIONAL REMARKS THAT DON'T HAVE A
     C*     CORRESPONDING KEY IN SDCMP OR THAT ARE INACTIVE
     C*
     C                   MOVEL     *LOVAL        CMKEY
     C                   MOVEL     *LOVAL        CMSEQ#
     C     ARKY1         SETLL     PAR01
     C                   READ      PAR01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   WRITE     PAR02
     C*
     C                   DELETE    PAR01
     C                   ELSE
     C*
     C     ARSTAT        IFEQ      'I'
     C                   WRITE     PAR02
     C*
     C                   DELETE    PAR01
     C                   ELSE
     C                   EXCEPT    RCM01
     C                   END
     C                   END
     C*
     C                   READ      PAR01                                  50
     C                   END
     C*** END   ADD *** 07/28/93 **************************************
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     O*** ADD ********* 07/28/93 **************************************
     OPCM01     E            RCM01
