     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *TITLE SC826R - OPTICAL PURGE/DELETE COPY TO OPTICAL PGM        *
      *****************************************************************
      *    PROGRAM DESCRIPTION:                                       *
      *        BACKUP AND BATCH - Optical Copy Documents to Optical   *
      *                           Program                             *
      *          Read in SMPWL1, for all records where PRSTAT = "P"   *
      *          (purge requested), read the record and create a file *
      *          on optical and copy the DLO to it.  Update the PW    *
      *          record must be done too.  Optical data is NOT written*
      *          until the close of file occurs.                      *
      *                                                               *
      *  CALLED FROM: SC825C                                          *
      *                                                               *
      *  CALLS TO: SCOCUPR1                                           *
      *            CPYTOOP1                                           *
      *            CRTOPTD                                            *
      *            SIMGOPT1                                           *
      *                                                               *
      *  CREATED: 03/31/03    PROGROGRAMMER: C:145    PROJECT: 124IAA *
      *****************************************************************
      ** REVISIONS:                                                   *
      *  CREATED:             PROGRAMMER:             PROJECT:        *
      *                                                               *
      *  03/31/02             C:145                   124IAA          *
      *                Original program for optical purge/delete      *
      *                copy program was SC856R.  Program renamed      *
      *                and modified to reflect changes for new        *
      *                batch/backup process.                          *
      ******************************************************************
      * INDICATOR USAGE                                                *
      *                                                                *
      ******************************************************************
      *
     FSMPWL1    UF   E           K DISK
     FSDCMLE    UF   E           K DISK
     FSMDNL1    UF A E           K DISK
     FSMOTL     UF   E           K DISK
     FSMOOL     UF A E           K DISK
     FSMMDL     IF   E           K DISK
     FSMMWL2    UF A E           K DISK
      *

     D ERM             S             80    DIM(5) CTDATA PERRCD(1)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  OTVOLI                 1      6
     D  ##VPRE                 1      2
     D  ##VOL#                 3      6  0
      *
     D                 DS
     D  OTBKVI                 1      6
     D  ##BKVP                 1      2
     D  ##BKV#                 3      6  0
      *
     D                 DS
     D  OTOFLD                 1      8
     D  ##FPRE                 1      4
     D  ##FLD#                 5      8  0
      *
     D                 DS
     D  OTBKCF                 1      8
     D  ##BKFP                 1      4
     D  ##BKF#                 5      8  0
      *
     D                 DS
     D  OTDCID                 1     12
     D  ##DOS                  1      3
     D  ##DC#                  4      8  0
     D  ##PWID                 9      9
     D  ##SD#                 10     12  0
      *
     D                 DS
     D  OTBKID                 1     12
     D  ##BKD                  1      3
     D  ##BD#                  4      8  0
     D  ##BKID                 9      9
     D  ##BS#                 10     12  0
      *
     D                 DS
     D  OTOSBD                 1     12
     D  ##SUBD                 1      8
     D  ##PWI2                 9      9
     D  ##CSBD                10     12  0
      *
     D                 DS
     D  OTBKSD                 1     12
     D  ##BKSD                 1      8
     D  ##BKP2                 9      9
     D  ##BKSB                10     12  0
      *
      *  Program status data structurs
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  Entry Parms
      *

     C     *ENTRY        PLIST
     C                   PARM                    BKUP              1
      *
      *  Key lists
      *
      *  Purge/delete transaction file
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     PWKY          KLIST
     C                   KFLD                    XXSTAT            1
      *
      *  Darts comment record
      *
     C     CMKY          KLIST
     C                   KFLD                    PWKEY            40
     C                   KFLD                    PWSEQ#            5 0
      *
      *  Document location file
      *
     C     DNKY          KLIST
     C                   KFLD                    PWKEY            40
     C                   KFLD                    PWSEQ#            5 0
      *
      *  Optical control file
      *
     C     OTKY          KLIST
     C                   KFLD                    QQOSYS
     C                   KFLD                    QQLIBN
      *
      *  Multiple objects file
      *
     C     MDKY          KLIST
     C                   KFLD                    MDKEY
     C                   KFLD                    MDSEQ#
     C                   KFLD                    MDSYSN
     C                   KFLD                    MDOFLR
     C                   KFLD                    MDOID
      *
      *  Parm lists
      *
      *    Parm list for call to optical copy program
      *
     C     PLOPT         PLIST
     C                   PARM                    SPATH            63
     C                   PARM                    TPATH            63
     C                   PARM                    QMSGD           256
     C                   PARM                    QQRTCD            1
      *
      *  Parm list for call to optical copy program for backup
      *
     C     BKOPT         PLIST
     C                   PARM                    SPATH            63
     C                   PARM                    BTPATH           63
     C                   PARM                    QMSGD           256
     C                   PARM                    BKRTCD            1
      *
      *  Parm list for call to create optical directory
      *
     C     PLCOD         PLIST
     C                   PARM                    NPATH            63
     C                   PARM                    QMDSC           256
     C                   PARM                    QQRTCD            1
      *
      *  Parm list for call to create optical directory for backup
      *
     C     BKCOD         PLIST
     C                   PARM                    BNPATH           63
     C                   PARM                    QMDSC           256
     C                   PARM                    BQRTCD            1
      *
      *  Process records selected for purge
      *
     C                   MOVE      'P'           XXSTAT                              PRGD RCDS
      *
      *  Optical info is currently specific to a single optical library
      *  We are using "G" for system and "OPT01LIB" for library name.
      *  We would have to create a method of selecting the SYS/LIB
      *  if we were to use more than one lib at a time.
      *
     C                   MOVE      'G'           QQOSYS            1
     C                   MOVEL     'OPTMLB01'    QQLIBN           10
      *
      * Call SCOCUPR1 to mark copy as started
      *
     C                   CALL      'SCOCUPR1'
     C                   PARM      'C'           XXOP              1
     C                   PARM      'B'           XXBA              1
     C                   PARM      *BLANKS       XXRTN             1
      *
     C     XXRTN         CABNE     *BLANKS       TERM
      *
      *  Read each record selected for purge
      *
     C     PWKY          CHAIN     PPW01                              60
      *
     C     *IN60         DOWEQ     '0'
      *
      *  Get Darts comment record here to find source path(SPATH)
      *
     C     CMKY          CHAIN     PCM01                              61
      *
     C     *IN61         IFEQ      '1'
     C                   MOVEL     Q1USER        PWUSER
     C                   MOVEL     Q1PGM         PWPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        PWTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PWDT
      *
     C                   MOVE      'E'           PWSTAT
     C                   MOVE      ERM(4)        PWERDS
     C                   UPDATE    PPW01
     C                   GOTO      NXTPW
      *
      *  Must have a CMT record or can't do purge - if error
      *  send message to QSYSOPR and exit.
      *
     C                   ELSE
     C     CMSTAT        IFEQ      'I'
     C                   MOVEL     Q1USER        PWUSER
     C                   MOVEL     Q1PGM         PWPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        PWTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PWDT
      *
     C                   MOVE      'E'           PWSTAT
     C                   MOVE      ERM(5)        PWERDS
     C                   UPDATE    PPW01
     C                   GOTO      NXTPW
     C                   END
     C                   ENDIF
      *
      * Since the QRYSLT that selects records to include in the
      * Purge Request file that may or may not be eligible to process
      * we need to test for that.
      *
     C     CMDCID        IFEQ      *BLANKS
     C     CMFL01        OREQ      'Y'
      *
      * If this remark has multiple objects attached to process them.
      * -ELSE- Do not process, get next record.
      *
     C     CMQPTY        IFEQ      'Y'
     C                   EXSR      MULTI
     C                   ELSE
     C                   GOTO      NXTPW
     C                   ENDIF
      *
      * This is a single remark, do regular process.
      *
     C                   ELSE
      *
      *  Get optical control record and lock it
      *
     C     OTKY          CHAIN     POT01                              62
      *
     C     *IN62         IFEQ      '1'
      *
      *  If no record, ERROR and can't continue
      *    Send message to QSYSOPR and exit
      *
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     GOTONE        IFEQ      *BLANK
     C                   MOVE      '1'           GOTONE            1
     C                   ENDIF
      *
      *  If purge process is not to run end this program.
      *
     C     OTPPFL        CABNE     'Y'           TERM
      *
      *  Verify that the # of docs/# of subdirs is within range
      *  If not, call appropriate routine to perform changes, etc.
      *
      *  If # of docs in the subdir is > OTM#DC, then call pgm to
      *  create a new subdirectory.
      *
     C     OTC#DC        CASGE     OTM#DC        NEWSD                          CREATE NEW SUBD
     C                   ENDCS
      *
      *  Verify that the # of docs/# of subdirs for backup is within
      *  range.  If not, call appropriate routine to perform changes.
      *
      *  If # of docs in the bkup subdir is > OTM#DC, then call pgm to
      *  create a new backup subdirectory.
      *
     C     OTBC#D        CASGE     OTM#DC        NWBKSD                         CREATE NEW BKUP SUBD
     C                   ENDCS
      *
      *  Create the source path
      *
     C     '/'           CAT(P)    CMFOLD:0      SPATH
     C                   CAT(P)    '/':0         SPATH
     C     CMIMGF        IFEQ      'Y'
     C                   CAT(P)    CMIDOC:0      SPATH
     C                   ELSE
     C                   CAT(P)    CMDCID:0      SPATH
     C                   ENDIF
      *
      * Build the DN document id.
      *
     C                   MOVEL     'DOS'         ##DOS
     C                   MOVEL     '.'           ##PWID
     C                   ADD       1             ##DC#
     C                   MOVE      ##CSBD        ##SD#
     C                   ADD       1             OTC#DC
      *
      * Build the backup DN document id.
      *
     C     BKUP          IFEQ      '1'
     C                   MOVEL     'BKD'         ##BKD
     C                   MOVEL     '.'           ##BKID
     C                   ADD       1             ##BD#
     C                   MOVE      ##CSBD        ##BS#
     C                   ADD       1             OTBC#D
     C                   ENDIF
      *
      *  Will make sure to update the current doc#, etc
      *
     C     RDYCPY        TAG
      *
      *  Move PW values to QQ(parm list) values for call
      *
      *  Create the target path
      *
     C     '/'           CAT(P)    OTVOLI:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OTOFLD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OTOSBD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OTDCID:0      TPATH
      *
      *  Create the backup target path
      *
     C     BKUP          IFEQ      '1'
     C     '/'           CAT(P)    OTBKVI:0      BTPATH
     C                   CAT(P)    '/':0         BTPATH
     C                   CAT(P)    OTBKCF:0      BTPATH
     C                   CAT(P)    '/':0         BTPATH
     C                   CAT(P)    OTBKSD:0      BTPATH
     C                   CAT(P)    '/':0         BTPATH
     C                   CAT(P)    OTBKID:0      BTPATH
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       QMSGD           256
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   MOVE      *BLANKS       BKRTCD            1
      *
      *  Call program to copy the DNS file to optical via copy stream  FILE
      *  API's
      *
     C                   CALL      'CPYTOOP1'    PLOPT
      *
      *  If unsuccessful
      *
     C     QQRTCD        IFNE      *BLANK
      *
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
      *
      *  If volume is full, get next volume, change the OT record,
      *  and path info for call to CPYSTRMFILE.
      *  Go to RDYCPY.
     C                   EXSR      NEWVL
     C                   GOTO      RDYCPY
     C                   ENDIF
      *
      *  Handle other errors as needed.
      *
     C                   MOVE      'E'           PWSTAT
     C     QQRTCD        IFEQ      '2'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(2)        PWERDS
     C                   ELSE
     C                   MOVEL     QMSGD         PWERDS
     C                   END
      *
     C                   END
      *
     C     QQRTCD        IFEQ      '3'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(3)        PWERDS
     C                   ELSE
     C                   MOVEL     QMSGD         PWERDS
     C                   END
      *
     C                   END
      *
      * Clear QMSGD so it will now hold backup message descriptions
      *
     C                   MOVE      *BLANKS       QMSGD           256
      *
     C     BKUP          IFEQ      '1'
     C                   CALL      'CPYTOOP1'    BKOPT
      *
      *  If unsuccessful
      *
     C     BKRTCD        IFNE      *BLANK
      *
     C     BKRTCD        CABEQ     '*'           TERM
     C     BKRTCD        IFEQ      '1'
      *
      *  If volume is full, get next volume, change the OT record,
      *  and path info for call to CPYSTRMFILE.
      *  Go to RDYCPY.
     C                   EXSR      NWBKVL
     C                   GOTO      RDYCPY
     C                   ENDIF                                                  BKRTCD = 1
      *
      *  Handle other errors as needed
      *
     C                   MOVE      'E'           PWSTAT
     C     BKRTCD        IFEQ      '2'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(2)        PWERDS
     C                   ELSE
     C                   MOVEL     QMSGD         PWERDS
     C                   END
      *
     C                   END                                                     BKRTCD = 2
      *
     C     BKRTCD        IFEQ      '3'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(3)        PWERDS
     C                   ELSE
     C                   MOVEL     QMSGD         PWERDS
     C                   END
      *
     C                   END                                                    BKRDCD = 3
      *
     C                   END                                                    BKRTCD NE BLANK
     C                   ENDIF                                                  BKUP EQ '1' NE BLANK
      *
      * Update the Optical Control File
      *
     C                   UPDATE    POT01
      *
     C                   ELSE
      *
      *  Mark this record as "C"omplete (IN PWSTAT)
      *
     C                   MOVE      'C'           PWSTAT
      *
      *  Write/Update DN file
      *
     C     DNKY          CHAIN     PDN01                              63
      *
     C                   MOVEL     OTDCID        DNDCID
     C                   MOVEL     QQOSYS        DNOSYS
     C                   MOVEL     OTOFLD        DNOFLD
     C                   MOVEL     OTOSBD        DNOSBD
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DNDTWR
      *
     C                   MOVE      DSTIME        DNTMWR
     C                   MOVEL     Q1USER        DNUSWR
      *
     C                   MOVE      'Y'           DNOPTF
      *
     C                   MOVEL     OTVOLI        DNVOL1
      *
     C                   MOVE      'A'           DNSTAT
     C                   MOVEL     Q1USER        DNUSER
     C                   MOVEL     Q1PGM         DNPGM
     C                   MOVE      DSTIME        DNTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DNDT
      *
      * Copy backup volume, subdir, and folder to DN file.
      *
     C     BKUP          IFEQ      '1'
     C                   MOVEL     OTBKID        DNBKID
     C                   MOVEL     OTBKCF        DNBKFD
     C                   MOVEL     OTBKSD        DNBKSD
     C                   MOVEL     OTBKVI        DNBKV1
     C                   ENDIF
      *
     C     *IN63         IFEQ      '0'
      *
      *  No tape info written
      *
     C                   UPDATE    PDN01
     C                   ELSE
     C                   MOVEL     PWKEY         DNKEY
     C                   MOVE      PWSEQ#        DNSEQ#
     C                   MOVEL     'G'           DNSYS
     C                   MOVE      *ZERO         DN#BYT                         LATER USE
     C                   MOVE      *ZEROS        DNPAGS                         LATER USE
      *
      *  No tape info written
      *
     C                   MOVEL     Q1USER        DNIUSR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DNIDT
      *
     C                   WRITE     PDN01
     C                   END
      *
      * Update the Optical Control File
      *
     C                   UPDATE    POT01
      *
      * If this remark has multiple objects attached, process them.
      *
     C     CMQPTY        IFEQ      'Y'
     C                   EXSR      MULTI
     C                   ENDIF
      *
     C                   ENDIF                                                  CMDCID End
      *
      *  Call to the optical update files for imaging
      *
     C                   CALL      'SIMGOPT1'                           99
     C                   PARM                    DNDCID
     C                   PARM                    DNOSYS
     C                   PARM                    DNOFLD
     C                   PARM                    DNOSBD
     C                   PARM                    DNKEY
     C                   PARM                    DNSEQ#
     C                   PARM                    DNSYS
     C                   PARM                    QQLIBN
     C                   PARM                    DNVOL1
      *
     C                   END
      *
      *  Other PW fields
      *
     C                   MOVEL     CMDCID        PWDCID
     C                   MOVEL     CMFOLD        PWFOLD
     C                   MOVEL     QQOSYS        PWOSYS
     C                   MOVEL     OTOSBD        PWOSBD
     C                   MOVEL     OTOFLD        PWOFLD
     C                   MOVEL     OTDCID        PWOPDI
     C                   MOVEL     OTBKSD        PWOBSD
     C                   MOVEL     OTBKCF        PWOBFL
     C                   MOVEL     OTBKID        PWOBDI
      *
     C     PWSTAT        IFEQ      'C'
     C                   MOVE      'D'           PWDELF
     C                   ENDIF
      *
     C                   MOVEL     Q1USER        PWUSER
     C                   MOVEL     Q1PGM         PWPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        PWTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PWDT
      *
      *  Update the PW file with "completed" info
      *
     C                   UPDATE    PPW01
      *
     C     PWSTAT        IFEQ      'C'
      *
      *  Update the CM record by setting CMFL05 = "O" stating document
      *  now resides on optical.
      *
     C                   MOVE      'O'           CMFL05
     C                   UPDATE    PCM01
      *
     C                   ENDIF
      *
     C     NXTPW         TAG
      *
     C     PWKY          READE     PPW01                                  60
      *
     C                   ENDDO
      *
      *  Call SCOCUPR1 to mark copy as completed
      *
     C     GOTONE        IFEQ      '1'
     C                   CALL      'SCOCUPR1'
     C                   PARM      'C'           XXOP              1
     C                   PARM      'A'           XXBA              1
     C                   PARM      *BLANKS       XXRTN             1
     C                   ENDIF
      *
     C     TERM          TAG
      *
      *  Get optical control (OT) record and shut off purge if not
      *  already.
      *
     C     OTKY          CHAIN     POT01                              62
      *
     C     *IN62         IFEQ      '0'
     C                   MOVE      'N'           OTPPFL
     C                   UPDATE    POT01
     C                   ENDIF
      *
     C                   MOVE      '1'           *INLR
      *
     C/EJECT
      *
      *====================================================================*
      *  NEWSD SUBROUTINE - CREATE A NEW SUBDIREC ON OPT         N E W S D *
      *====================================================================*
      *
     C     NEWSD         BEGSR
      *
      *  First check the maximum # of subdirectories has not been
      *  exceeded.  If not, create a new subdirectory in an existing
      *  folder, else create a new folder AND a new subdirectory.
      *  In all cases, update the OT record, clearing the last DOCID,
      *  the current subdirectory, and if applicable, the current
      *  folder name.
      *
     C     OTC#SD        IFGE      OTM#SD                                        (USE XX FOR #)
      *
      *  Change folder name in OT file, create new one on OPT volume
     C                   ADD       1             ##FLD#
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTOFLD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
      *
     C                   CALL      'CRTOPTD'     PLCOD
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
      *
      *  Change subdirectory to 001
      *
     C                   Z-ADD     1             OTC#SD
     C                   MOVE      001           ##CSBD
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTOFLD:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTOSBD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
      *
     C                   CALL      'CRTOPTD'     PLCOD
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
      *
      *  Change the DOCID to DOS00000.XXX
      *
     C                   MOVE      *ZEROS        ##DC#
     C                   MOVE      ##CSBD        ##SD#
      *
      *  Current # of docs in subdirectory
      *
     C                   Z-ADD     *ZEROS        OTC#DC
      *
     C                   ELSE
      *
      *  Add 1 to the # of subdirectories and update the OT file with
      *  the new SUBDIREC.XXX, current # + 1, reset the DOCID, and
      *  current number to 0.
      *
     C                   ADD       1             OTC#SD
     C                   ADD       1             ##CSBD
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTOFLD:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTOSBD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
      *
     C                   CALL      'CRTOPTD'     PLCOD
      *
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
      *
      *  Change DOC to DOS00000.XXX
      *
     C                   MOVE      *ZEROS        ##DC#
     C                   MOVE      ##CSBD        ##SD#
      *
      *  Current # of docs in subdirectory
      *
     C                   Z-ADD     *ZEROS        OTC#DC
      *
     C                   END
      *
     C                   UPDATE    POT01
     C     OTKY          CHAIN     POT01                              66
      *
     C     ENEWSD        ENDSR
      *
     C/EJECT
      *
      *====================================================================*
      *  NEWBKSD SUBROUTINE - CREATE A NEW BACKUP SUBDIREC ON OPT          *
      *====================================================================*
      *
     C     NWBKSD        BEGSR
     C*
      *
      *  First check the maximum # of backup subdirectories has not
      *  been exceeded.  If not, create a new backup subdirectory in an
      *  existing folder, else create a new backup folder AND a new
      *  backup subdirectory.
      *  In all cases, update the OT record, clearing the last DOCID,
      *  the current backup subdirectory, and if applicable, the
      *  backup folder name.
      *
     C     OTBC#S        IFGE      OTM#SD                                        (USE XX FOR #)
      *
      *  Change folder name in OT file, create new one on OPT volume
      *
     C                   ADD       1             ##BKF#
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTBKVI:0      BNPATH
     C                   CAT(P)    '/':0         BNPATH
     C                   CAT(P)    OTBKCF:0      BNPATH
     C                   MOVE      *BLANK        BKRTCD            1
     C                   MOVE      *BLANK        QBMDSC            1
      *
     C                   CALL      'CRTOPTD'     BKCOD
     C     BKRTCD        CABEQ     '*'           TERM
     C     BKRTCD        IFEQ      '1'
     C                   END
     C     BKRTCD        IFEQ      '2'
     C                   END
     C     BKRTCD        IFEQ      '3'
     C                   END
      *
      *  Change the backup subdirectory to 001
      *
     C                   Z-ADD     1             OTBC#S
     C                   MOVE      001           ##BKSB
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTBKVI:0      BNPATH
     C                   CAT(P)    '/':0         BNPATH
     C                   CAT(P)    OTBKCF:0      BNPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTBKSD:0      NPATH
     C                   MOVE      *BLANK        BKRTCD
     C                   MOVE      *BLANK        QBMDSC
      *
     C                   CALL      'CRTOPTD'     BKCOD
     C     BKRTCD        CABEQ     '*'           TERM
     C     BKRTCD        IFEQ      '1'
     C                   END
     C     BKRTCD        IFEQ      '2'
     C                   END
     C     BKRTCD        IFEQ      '3'
     C                   END
      *
      *  Change DOCID to DOS00000.XXX
      *
     C                   MOVE      *ZEROS        ##BD#
     C                   MOVE      ##BKSB        ##BS#
      *
      * CURRENT # OF DOCS IN SUBDIRECTORY
     C                   Z-ADD     *ZEROS        OTBC#D
      *
     C                   ELSE
      *
      *  Add 1 to # of subdirectories and update the OT file with
      *  the new SUBDIREC.XXX, current # + 1, reset the DOCID, and
      *  current number to 0.
      *
     C                   ADD       1             OTBC#S
     C                   ADD       1             ##BKSB
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTBKVI:0      BNPATH
     C                   CAT(P)    '/':0         BNPATH
     C                   CAT(P)    OTBKCF:0      BNPATH
     C                   CAT(P)    '/':0         BNPATH
     C                   CAT(P)    OTBKSD:0      BNPATH
     C                   MOVE      *BLANK        BKRTCD
     C                   MOVE      *BLANK        QBMDSC
      *
     C                   CALL      'CRTOPTD'     BKCOD
      *
     C     BKRTCD        CABEQ     '*'           TERM
     C     BKRTCD        IFEQ      '1'
     C                   END
     C     BKRTCD        IFEQ      '2'
     C                   END
     C     BKRTCD        IFEQ      '3'
     C                   END
      *
      *  Change the DOCID to DOS00000.XXX
      *
     C                   MOVE      *ZEROS        ##BD#
     C                   MOVE      ##BKSB        ##BS#
      *
      *  Current # of docs in subdirectory
      *
     C                   Z-ADD     *ZEROS        OTBC#D
      *
     C                   END
      *
     C                   UPDATE    POT01
     C     OTKY          CHAIN     POT01                              66
      *
     C     ENBKSD        ENDSR
      *
     C/EJECT
      *
      *====================================================================*
      *  NEWVL SUBROUTINE - CHANGE TO A NEW VOLUME ON OPT        N E W V L *
      *====================================================================*
      *
     C     NEWVL         BEGSR
      *
      *  Volume is full. We can change the program to use the next
      *  volume.  We may need to send a message to the operator to
      *  remind them that a new volume is required.  If it isn't
      *  loaded then it will allow the user to operator to create/load
      *  the volume.
      *
      *  We will need to set the OT record with the new volume id,
      *  reset the folder, subdirectory, etc, (# of docs, etc.)
      *  We will also have to create the folder/subdirectory, etc.
      *  on the platter.
      *
     C                   ADD       1             ##VOL#
      *
      *  Change folder name in OT file, create  new one on optical
      *  volume
      *
     C                   Z-ADD     1             ##FLD#
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTOFLD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
      *
     C                   CALL      'CRTOPTD'     PLCOD
      *
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
      *
      *  Change the subdirectory to 001
      *
     C                   Z-ADD     1             OTC#SD
     C                   MOVE      001           ##CSBD
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTVOLI:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTOFLD:0      NPATH
     C                   CAT(P)    '/':0         NPATH
     C                   CAT(P)    OTOSBD:0      NPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
      *
     C                   CALL      'CRTOPTD'     PLCOD
      *
     C     QQRTCD        CABEQ     '*'           TERM
     C     QQRTCD        IFEQ      '1'
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   END
      *
      *  Change DOCID to DOS00000.XXX
      *
     C                   MOVE      *ZEROS        ##DC#
     C                   MOVE      ##CSBD        ##SD#
      *
      * CURRENT # OF DOCS IN SUBDIRECTORY
      *
     C                   Z-ADD     *ZEROS        OTC#DC
      *
     C                   UPDATE    POT01
     C     OTKY          CHAIN     POT01                              66
      *
     C     ENEWVL        ENDSR
      *
      *====================================================================*
      *  NWBKVL SUBROUTINE - CHANGE TO A NEW VOLUME ON OPT NWBKVL          *
      *====================================================================*
      *
     C     NWBKVL        BEGSR
      *
      *  Backup volume is full. We can change the program to use the
      *  next volume.  We may need to send a message to the operator to
      *  remind them that a new volume is required.  If it isn't
      *  loaded then it will allow the user to operator to create/load
      *  the volume.
      *
      *  We will need to set the OT record with the new backup volume
      *  id, reset the backup folder, subdirectory, etc, (# of docs,etc.)
      *  We will also have to create the backup folder/subdirectory,
      *  etc. on the platter.
      *
     C                   ADD       1             ##BKV#
      *
      *  Change backup folder name in OT file, create new one on
      *  optical volume
      *
     C                   Z-ADD     1             ##BKF#
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTBKVI:0      BNPATH
     C                   CAT(P)    '/':0         BNPATH
     C                   CAT(P)    OTBKCF:0      BNPATH
     C                   MOVE      *BLANK        BKRTCD
     C                   MOVE      *BLANK        QMDSC
      *
     C                   CALL      'CRTOPTD'     BKCOD
      *
     C     BKRTCD        CABEQ     '*'           TERM
     C     BKRTCD        IFEQ      '1'
     C                   END
     C     BKRTCD        IFEQ      '2'
     C                   END
     C     BKRTCD        IFEQ      '3'
     C                   END
      *
      *  Change the backup subdirectory to 001
      *
     C                   Z-ADD     1             OTBC#S
     C                   MOVE      001           ##BKSD
      *
      *  Create the new path
      *
     C     '/'           CAT(P)    OTBKVI:0      BNPATH
     C                   CAT(P)    '/':0         BNPATH
     C                   CAT(P)    OTBKCF:0      BNPATH
     C                   CAT(P)    '/':0         BNPATH
     C                   CAT(P)    OTBKSD:0      BNPATH
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QMDSC
      *
     C                   CALL      'CRTOPTD'     BKCOD
      *
     C     BKRTCD        CABEQ     '*'           TERM
     C     BKRTCD        IFEQ      '1'
     C                   END
     C     BKRTCD        IFEQ      '2'
     C                   END
     C     BKRTCD        IFEQ      '3'
     C                   END
      *
      *  Current # of docs in backup subdirectory
      *
     C                   Z-ADD     *ZEROS        OTBC#D
      *
     C                   UPDATE    POT01
     C     OTKY          CHAIN     POT01                              66
      *
     C     ENWBKV        ENDSR
      *
      *********** ADD 05/17/00 ***************************************
      *====================================================================*
      *  MULTI SUBROUTINE - Process Multiple objects                   V L *
      *====================================================================*
      *
     C     MULTI         BEGSR
      *
      * Read each record with a key that is equal to the remark key.
      *
     C     DNKY          CHAIN     PMD01                              70
      *
     C     *IN70         DOWEQ     *OFF
      *
      *  Get Optical Control Record and lock it
      *
     C     OTKY          CHAIN     POT01                              62
      *
     C     *IN62         IFEQ      '1'
      *
      *  If no record, ERROR - can't continue
      *
      *   Send message to QSYSOPR and exit
      *
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     GOTONE        IFEQ      *BLANK
     C                   MOVE      '1'           GOTONE            1
     C                   ENDIF
      *
      *  If purge process is not to run, end this program
      *
     C     OTPPFL        CABNE     'Y'           TERM
      *
      *  Verify that the # of docs/# of subdirs is within range
      *  If not, call appropriate routine to perform changes etc.
      *
      *  If # of docs in the subdir is > OCM#DC, then call pgm to
      *  create a new subdirectory.
      *
     C     OTC#DC        CASGE     OTM#DC        NEWSD                          CREATE NEW SUBD
     C                   ENDCS
      *
      *  Verify that the # of docs/# of subdirs for backup is within
      *  range.  If not, call appropriate routine to perform changes.
      *
      *  If # of docs in the bkup subdir is > OTM#DC, then call pgm to
      *  create a new backup subdirectory.
      *
     C     OTBC#D        CASGE     OTM#DC        NWBKSD                         CREATE NEW BKUP SUBD
     C                   ENDCS
      *
      *  Create the SRC path
      *
     C     '/'           CAT(P)    MDOFLR:0      SPATH
     C                   CAT(P)    '/':0         SPATH
     C                   CAT(P)    MDOID:0       SPATH
      *
      * Build the OO document id.
      *
     C                   MOVEL     'DOS'         ##DOS
     C                   MOVEL     '.'           ##PWID
     C                   ADD       1             ##DC#
     C                   MOVE      ##CSBD        ##SD#
     C                   ADD       1             OTC#DC
      *
      * Build the backup OO document id.
      *
     C     BKUP          IFEQ      '1'
     C                   MOVEL     'BKD'         ##BKD
     C                   MOVEL     '.'           ##BKID
     C                   ADD       1             ##BD#
     C                   MOVE      ##CSBD        ##BS#
     C                   ADD       1             OTBC#D
     C                   ENDIF
      *
     C     RDYCP1        TAG
      *
      *  Move PW values to QQ(PARM LIST) values for call
      *
      *  Create the target path
      *
     C     '/'           CAT(P)    OTVOLI:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OTOFLD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OTOSBD:0      TPATH
     C                   CAT(P)    '/':0         TPATH
     C                   CAT(P)    OTDCID:0      TPATH
      *
      *  Create the backup target path
      *
     C     BKUP          IFEQ      '1'
     C     '/'           CAT(P)    OTBKVI:0      BTPATH
     C                   CAT(P)    '/':0         BTPATH
     C                   CAT(P)    OTBKCF:0      BTPATH
     C                   CAT(P)    '/':0         BTPATH
     C                   CAT(P)    OTBKSD:0      BTPATH
     C                   CAT(P)    '/':0         BTPATH
     C                   CAT(P)    OTDCID:0      BTPATH
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       QMSGD           256
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   MOVE      *BLANKS       BKRTCD            1
      *
      *  Call program to copy the DOS file to OPTICAL via copy stream  FILE
      *  API'S
      *
     C                   CALL      'CPYTOOP1'    PLOPT
      *
      *  If successful
     C     QQRTCD        IFNE      *BLANK
      *
     C     QQRTCD        CABEQ     '*'           TERM
      *
     C     QQRTCD        IFEQ      '1'
      *
      *  If vol full, get next volume, change the OT rcd,
      *  and path info for call to CPYSTRMFILE.
      *  Go to RDYCP1.
      *
     C                   EXSR      NEWVL
     C                   GOTO      RDYCP1
     C                   ENDIF
      *
      *  Handle other errors as needed.
      *
     C                   MOVE      'E'           MWSTAT
     C     QQRTCD        IFEQ      '2'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(2)        MWERDS
     C                   ELSE
     C                   MOVEL     QMSGD         MWERDS
     C                   END
      *
     C                   END
      *
     C     QQRTCD        IFEQ      '3'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(3)        MWERDS
     C                   ELSE
     C                   MOVEL     QMSGD         MWERDS
     C                   END
      *
     C                   END
      *
      * Clear QMSGD so it will now hold backup message descriptions
      *
     C                   MOVE      *BLANKS       QMSGD           256
      *
     C     BKUP          IFEQ      '1'
     C                   CALL      'CPYTOOP1'    BKOPT
     C*                    ENDIF
      *
      *  If unsuccessful
      *
     C     BKRTCD        IFNE      *BLANK
      *
     C     BKRTCD        CABEQ     '*'           TERM
     C     BKRTCD        IFEQ      '1'
      *
      *  If volume is full, get next volume, change the OT record,
      *  and path info for call to CPYSTRMFILE.
      *  Go to RDYCPY.
     C                   EXSR      NWBKVL
     C                   GOTO      RDYCPY
     C                   ENDIF                                                  BKRTCD = 1
      *
      *  Handle other errors as needed
      *
     C                   MOVE      'E'           PWSTAT
     C     BKRTCD        IFEQ      '2'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(2)        PWERDS
     C                   ELSE
     C                   MOVEL     QMSGD         PWERDS
     C                   END
      *
     C                   END                                                     BKRTCD = 2
      *
     C     BKRTCD        IFEQ      '3'
     C     QMSGD         IFEQ      *BLANKS
     C                   MOVE      ERM(3)        PWERDS
     C                   ELSE
     C                   MOVEL     QMSGD         PWERDS
     C                   END
      *
     C                   END                                                    BKRDCD = 3
      *
     C                   END                                                    BKRTCD NE BLANKS
     C                   ENDIF                                                  BKUP = '1'
      *
      * Update the Optical Control File
      *
     C*                    UPDATPOT01
      *
     C                   ELSE
      *
      *  Mark this record as "C"complete (in MWSTAT)
      *
     C                   MOVE      'C'           MWSTAT
      *
      *  Write/update OO file
      *
     C     MDKY          CHAIN     POO01                              71
      *
     C                   MOVEL     OTDCID        OODCID
     C                   MOVEL     QQOSYS        OOOSYS
     C                   MOVEL     OTOFLD        OOOFLD
     C                   MOVEL     OTOSBD        OOOSBD
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OODTWR
      *
     C                   MOVE      DSTIME        OOTMWR
     C                   MOVEL     Q1USER        OOUSWR
      *
     C                   MOVE      'Y'           OOOPTF
      *
     C                   MOVEL     OTVOLI        OOVOL1
      *
     C                   MOVE      'A'           OOSTAT
     C                   MOVEL     Q1USER        OOUSER
     C                   MOVEL     Q1PGM         OOPGM
     C                   MOVE      DSTIME        OOTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OODT
      *
      * Copy backup volume, subdir, and folder to OO file.
      *
     C     BKUP          IFEQ      '1'
     C                   MOVEL     OTBKID        OOBKID
     C                   MOVEL     OTBKCF        OOBKFD
     C                   MOVEL     OTBKSD        OOBKSD
     C                   MOVEL     OTBKVI        OOBKV1
     C                   ENDIF
      *
     C     *IN71         IFEQ      '0'
      *
      *  No tape info written
      *
     C                   UPDATE    POO01
      *
     C                   ELSE
      *
     C                   MOVEL     PWKEY         OOKEY
     C                   MOVE      PWSEQ#        OOSEQ#
     C                   MOVEL     MDSYSN        OOSYSN
     C                   MOVEL     MDOFLR        OOOFLR
     C                   MOVEL     MDOID         OOOID
     C                   MOVEL     'G'           OOSYS
     C                   MOVE      *ZERO         OO#BYT                         LATER USE
     C                   MOVE      *ZEROS        OOPAGS                         LATER USE
      *
      *  No tape info written
     C                   MOVEL     Q1USER        OOIUSR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OOIDT
      *
     C                   WRITE     POO01
     C                   END
      *
      *  Write/update MW file
      *
     C     MDKY          CHAIN     PMW01                              72
      *
     C                   MOVEL     OTDCID        MWOPDI
     C                   MOVEL     QQOSYS        MWOSYS
     C                   MOVEL     OTOFLD        MWOFLD
     C                   MOVEL     OTOSBD        MWOSBD
      *
     C                   MOVEL     Q1USER        MWUSER
     C                   MOVEL     Q1PGM         MWPGM
     C                   MOVE      DSTIME        MWTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MWDT
      *
     C     BKUP          IFEQ      '1'
     C                   MOVEL     PWOBDI        MWOBDI
     C                   MOVEL     PWOBFL        MWOBFL
     C                   MOVEL     PWOBSD        MWOBSD
     C                   ENDIF
      *
     C     MWSTAT        IFEQ      'C'
     C                   MOVEL     'D'           MWDELF
     C                   ENDIF
      *
     C     *IN72         IFEQ      '0'
      *
     C                   UPDATE    PMW01
      *
     C                   ELSE
      *
     C                   MOVEL     PWKEY         MWKEY
     C                   MOVE      PWSEQ#        MWSEQ#
     C                   MOVEL     MDSYSN        MWSYSN
     C                   MOVEL     MDOFLR        MWOFLR
     C                   MOVEL     MDOID         MWOID
     C                   MOVEL     'G'           MWSYS
     C                   MOVE      *BLANKS       MWHCMD
     C                   MOVE      *BLANKS       MWCPRY
     C                   MOVE      *BLANKS       MWFOLD
     C                   MOVE      *BLANKS       MWSUBD
     C                   MOVE      *BLANKS       MWPNAM
     C                   MOVE      *BLANKS       MWASTT
     C                   MOVE      *ZERO         MWBUL#
     C                   MOVE      *BLANKS       MWTSFL
      *
     C                   MOVEL     Q1USER        MWIUSR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MWIDT
      *
     C                   WRITE     PMW01
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   UPDATE    POT01
      *
     C     DNKY          READE     PMD01                                  70
     C                   ENDDO
     C                   ENDSR
      *====================================================================*
      ********END ADD 05/17/00 ***************************************
**
Optical Volume is Full(Threshold Percent has been reached).
ERROR - Path Name used is not valid.
ERROR - Document not written to Optical Library.  See Job Log for details.
ERROR - Matching DARTS comment record could not be found.
ERROR - Matching DARTS comment record has been inactivated.
