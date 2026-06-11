     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
     H/TITLE SC611R - Objest Attachment Delete                        *
      *****************************************************************
      * Indicator Usage                                               *
      * SCR01:                                                        *
      *  22    INVALID OPTION ENTERED                                 *
      *  32    ROLL UP INDICATOR                                      *
      *  50    READ ON SCR01                                          *
      *  60    READ ON PCM01                                          *
      *  61    CHAIN ON PMD01                                         *
      *  62    CHAIN ON PKD01                                         *
      *****************************************************************
     FSC611F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F                                     SFILE(SCR02S:RRN3)
      *
     FSDCMLW    UF   E           K DISK
     FSMMDL1    IF   E           K DISK
     FSMMDL     UF   E           K DISK
     F                                     RENAME(PMD01:PMD02)
     FSMKDL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  DUMMY                  1     75
     D  INDENT                 1      5
     D  OBJFLR                 6     75
      *
      * Named Constants
      *
     D CLAIM           C                   CONST('Claimant:  ')
     D EMPLOY          C                   CONST('Employer:  ')
     D AGENT           C                   CONST('Agent:  ')
     D NORCDS          C                   CONST('* * * No Records to-
     D                                      Display * * *')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   Key List for the Remark Detail File
      *

     C     CMKY          KLIST
     C                   KFLD                    QQKEY
      *
      *   Key List for the Multi-Object File
      *
     C     MDKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
      *
      *   Key List for the Key Definition File
      *
     C     KDKY          KLIST
     C                   KFLD                    QQPNAM
      *
      *   Key List for the Delete Sub File
      *
     C     SFKY          KLIST
     C                   KFLD                    KEY
     C                   KFLD                    SEQ
     C                   KFLD                    OID
     C                   KFLD                    OFLR
      *
      *
      *   Key List to update CM File
      *
     C     SF2KY         KLIST
     C                   KFLD                    KEY
     C                   KFLD                    SEQ
      *
      *------------------------*
      *   Parameter List for Called program
      *------------------------*
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD            1
      *
      *   PList for the Build Name Program
      *
     C     PGMLST        PLIST
     C                   PARM                    QQPNAM           10
     C                   PARM                    Q1KEY            79
     C                   PARM                    Q1NAME           79
      *
     C                   TIME                    DSDTTM
      *
     C     *LIKE         DEFINE    RRN2          SRRN2
      *
      *
      *
      *------------------------*
      *** Reset Point in Program
      *------------------------*
     C     TOP           TAG
      *
      *   Initialize Routine * * *
     C                   EXSR      INIT
      *
     C     CMKY          SETLL     PCM01                              60
      *
      *** Clear Subfile
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN04
      *
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
      *
     C     STRT          TAG
      *
     C                   MOVEL     Q2FILE        SCFILE           10            SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF            8             FOR DISPLAY
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      *OFF          *IN01
     C                   END
      *
      *******************************************************************
      *   Process all Command Keys
      *******************************************************************
      *
     C     *IN11         IFEQ      '1'
     C****************************************
      *   F3 = Exit                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'C'           QQRTCD
     C                   EXSR      ENDPGM
     C                   END                                                    END *INKC
      ****************************************
      *   F12 = Previous                     *
      ****************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'L'           QQRTCD
     C                   EXSR      ENDPGM
     C                   END                                                    END *INKL
      ****************************************
      *   *IN18 -  Rollup Request            *
      ****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
      *
      *
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
      **************************************************************************
      ***********************************
     C     SCR01T        TAG
      ***********************************
      *
      *
     C* * * Edit Fields
     C                   MOVE      *OFF          ERR
     C*
     C*** Save RRN so that rolling can properly function if completion is marked
     C                   MOVE      RRN2          SRRN2
      *
     C                   READC     SCR01S                                 50
      *
     C     *IN50         DOWEQ     '0'
      *
      *** Process 4=Delete
     C     SCSEL         IFEQ      '4'
     C                   MOVEA     '000'         *IN(20)
      *
     C****************     MOVE *BLANK    SCSEL
     C                   UPDATE    SCR01S
      *
      *
      *  Write the attached objects to the delete Subfile
      *
     C                   ADD       1             RRN3
     C                   WRITE     SCR02S
      *
     C                   END                                                    END SEL EQ '5'
      *
      *
      *
     C                   MOVEA     '000'         *IN(20)
     C     SCSEL         IFNE      ' '
     C     SCSEL         IFNE      '4'
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           ERR               1
     C                   UPDATE    SCR01S
     C                   END
     C                   END
     C*
     C     NXT1          TAG
     C*
     C                   READC     SCR01S                                 50
     C                   END
     C*
     C*** Restore RRN so that rolling can properly function
     C                   MOVE      SRRN2         RRN2
     C*
     C     ERR           CABEQ     '1'           STRT
      *
     C     RRN3          IFGT      *ZERO
     C                   EXSR      DELETE
     C                   ENDIF
      *
     C                   GOTO      STRT
     C*
      *
      *==============================================================
      * * *   End Program Subroutine
      *==============================================================
     C     ENDPGM        BEGSR
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDSR
      *
      *====================================================================*
      * * *   Init Subroutine                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     *ZERO         RRN2              5 0
     C                   Z-ADD     *ZERO         RRN3              5 0
     C                   MOVE      *BLANK        PREFIX
     C                   MOVE      *BLANK        SCDESC
      *
      * Get Name for Heading
      *
     C     QQPNAM        CHAIN     PKD01                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     QQKEY         Q1KEY
      *
     C     KDCVTP        IFNE      *BLANKS
     C                   SELECT
     C     KDCVTP        WHENEQ    'CR001R'
     C                   MOVEL     CLAIM         PREFIX           10
     C     KDCVTP        WHENEQ    'CR002R'
     C                   MOVEL     EMPLOY        PREFIX
     C     KDCVTP        WHENEQ    'CR003R'
     C                   MOVEL     AGENT         PREFIX
     C                   ENDSL
     C                   CALL      KDCVTP        PGMLST
     C     PREFIX        CAT       Q1NAME:2      SCDESC
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF           10
     C                   MOVEL     'SCR01'       Q2RECF
      *                                                            TOGGLE
     C                   ENDSR
      *====================================================================*
      * * *   Load Subroutine                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
      *
     C                   MOVE      *OFF          *IN32
     C                   MOVE      *OFF          *IN20
      *
      *  *  *  *  Load the Subfile Screen with all records
      *
     C     QQKEY         READE     PCM01                                  60
      *
     C     *IN60         IFEQ      *ON
     C     RRN2          ANDLT     1
     C                   MOVE      *ON           *IN32                          Roll up/dwn
     C                   END                                                    error
      *
     C     *IN60         DOWEQ     *OFF
      *
      *  Write the attached objects to the Subfile
      *
     C     MDKY          CHAIN     PMD01                              61
      *
      *  Write the major remark to the Subfile
      *
     C     *IN60         IFEQ      *OFF
     C     *IN61         ANDEQ     *OFF
     C                   ADD       1             RRN2
     C                   MOVE      *ON           *IN10
     C                   MOVEL     CMCOMM        SCCOMM
     C                   MOVEL     CMKEY         KEY
     C                   MOVE      CMSEQ#        SEQ
     C                   WRITE     SCR01S
     C                   MOVE      *OFF          *IN10
      *
     C     *IN61         DOWEQ     *OFF
     C                   ADD       1             RRN2
     C                   MOVEL     MDOID         OID
     C                   MOVEL     MDOFLR        OFLR
     C                   MOVE      *BLANKS       DUMMY
     C                   MOVE      *BLANKS       INDENT
     C     MDOID         CAT       '\':0         OBJFLR
     C                   CAT       MDOFLR:0      OBJFLR
     C                   MOVEL     DUMMY         SCCOMM
     C                   WRITE     SCR01S
     C     MDKY          READE     PMD01                                  61
     C                   ENDDO
     C                   END
      *
     C     QQKEY         READE     PCM01                                  60
      *
     C                   ENDDO
      *
      *
      * * * Initialize Subfile if no records to display
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     NORCDS        SCCOMM
      *
     C                   Z-ADD     1             RRN2
     C                   WRITE     SCR01S
     C                   Z-ADD     RRN2          #@$2
     C                   ELSE
      *
     C*** To position screen and cursor
     C                   Z-ADD     2             #@$2
     C                   END
      *
     C                   ENDSR
      *====================================================================*
      * * *   Delete Subroutine                              D E L E T E ***
      *====================================================================*
     C     DELETE        BEGSR
     C                   MOVEL     Q2FILE        SCFILE           10            SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF            8             FOR DISPLAY
      *
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      *ON           *IN02
     C                   WRITE     SCR01
     C                   EXFMT     SCR02C
     C                   MOVE      *OFF          *IN02
     C                   END
      *
      *******************************************************************
      *   Process all Command Keys
      *******************************************************************
      *
     C     *IN11         IFEQ      '1'
     C****************************************
      *   F3 = Exit                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'C'           QQRTCD
     C                   EXSR      ENDPGM
     C                   END                                                    END *INKC
      ****************************************
      *   F12 = Previous                     *
      ****************************************
     C     *INKL         IFEQ      '1'
      *
      *** Clear Subfile
     C                   MOVE      *ON           *IN05
     C                   WRITE     SCR02C
     C                   MOVE      *OFF          *IN05
     C                   MOVE      *OFF          *IN11
     C                   MOVE      *OFF          *INKL
     C                   Z-ADD     *ZERO         RRN3
     C                   MOVEL     'SCR01C'      Q2RECF                          FOR DISPLAY
     C                   ENDIF                                                  END *INKL
      *
     C                   ELSE
      *
      *  Delete the attached objects from the file and return to
      *  the calling program.
      *
     C     1             DO        999           X                 5 0
     C     X             CHAIN     SCR02S                             62
     C     SCSEL         IFEQ      '4'
     C     SFKY          CHAIN     PMD02                              63
      *
     C     *IN62         IFEQ      *OFF
     C     *IN63         ANDEQ     *OFF
     C                   DELETE    PMD02
      *
      * If there are no more attachments for this remark, update the
      * CMQPTY field with blanks.
      *
     C     SF2KY         CHAIN     PMD01                              64
      *
     C     *IN64         IFEQ      *ON                                          No rcd found
     C     SF2KY         CHAIN     PCM01                              65
      *
     C     *IN65         IFEQ      *OFF
     C                   MOVE      *BLANKS       CMQPTY
     C                   UPDATE    PCM01
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
     C                   MOVE      *BLANK        SCSEL
     C                   EXSR      ENDPGM
      *
     C                   ENDIF                                                  END *IN11
      *
     C                   ENDSR
