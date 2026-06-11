     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     SC930R                                        **
     H* DESCRIPTION: PURGE ROUTINE - WRITE TO PURGE FILES          **
     H*                                                            **
     H* CREATED BY:  WENDY DILLARD        DATE:  03/31/92          **
     H*            BASED ON FEE SCHEDULE PURGE FILE:               **
     H*              FPRG2R                                        **
     H***************************************************************
     H* REVISIONS:                                                 **
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
     H* SDCMLB:  REMARK DETAIL FILE (KEY)                          **
     H* PDCMP:  SAVE FILE                                          **
     H* PDCML:   LOGICAL OVER SAVE FILE (KEY/SEQ#/ASQ#)            **
     H* SMARL1:  ADDITIONAL REMARKS FILE  (KEY/SEQ#/ASQ#)          **
     H* PMARP:  SAVE FILE                                          **
     H* SMDLL:   DOCUMENT LOCATION FILE  (KEY/SEQ#)                **
     H***************************************************************
     FSDCMLB    IF   E           K DISK
     FPDCMP     O    E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FSMARL1    IF   E           K DISK
     FPMARP     O    E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FPMARL     IF   E           K DISK
     F                                     RENAME(PAR01:PAR03)
     FSMDLL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    I*  Rename the duplicate field names.
    I*
     Ipcm01
     I              cmuncn                      oxuncn
     I              cmshrn                      oxshrn
     I              cmifsp                      oxifsp

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
     C*
     C*  ADDITIONAL REMARKS PURGE FILE
     C*
     C     ARKYP         KLIST
     C                   KFLD                    ARKEY
     C                   KFLD                    ARSEQ#
     C                   KFLD                    ARASQ#
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
     C*
     C                   TIME                    DSDTTM
     C*
     C*   READ REMARK DETAIL RECORD
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
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
     C     ARKYP         CHAIN     PAR03                              51
     C     *IN51         IFEQ      '1'
     C     ARDT          ANDLT     CMDT
     C                   WRITE     PAR02                                        PURGE FILE
     C                   END
     C*
     C     ARKY1         READE     PAR01                                  50
     C                   END                                                    DOWEQ PAR01
     C*
     C*   WRITE REMARK DETAIL RECORD IF THIS IS AN INACTIVE REMARK
     C*
     C                   WRITE     PCM02                                        PURGE FILE
     C*
     C                   ELSE                                                   STAT NE 'I'
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
     C                   WRITE     PAR02                                        PURGE FILE
     C*
     C     ARKY1         READE     PAR01                                  50
     C                   END                                                    DOWEQ PAR01
     C*
     C*   PROCESS REMARK DETAIL RECORD
     C*
     C                   WRITE     PCM02                                        PURGE FILE
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
     C                   END                                                    END STAT = I
     C*
     C     SKIP          TAG
     C*
     C*** READ NEXT RECORD
     C                   READ      PCM01                                  60
     C                   END                                                    ENDDOW *IN60
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
