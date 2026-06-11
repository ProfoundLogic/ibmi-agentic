     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W9120R - UDS Claims Import prompt
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  This program will prompt for Grp/EMP.          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 04/20/10  PROGRAMMER: C:232 PROJECT: 114WWA         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
     FW9120F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMEMP     UF A E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDSY)
     FWMUPL     IF   E           K DISK
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSY          DS
     D  DBRRNY               397    400B 0
     D*
     D* Stand Alone Fields
     D*
     DFOLDER           S             80    INZ('UDS')
     DFOLDERAR         S             80    INZ('UDS/ARCHIVE')
     DFULLFOLDER       S             80
     DFOLDER_SAVE      S             80
     DPREFIX           S             80
     DSUFFIX           S              3
     D IFSOBJ          S            256A   INZ
     D IFSOBJAR        S            256A   INZ
     D IFSOBJFND       S               n   INZ
     D xxcmd           s            200    inz(*blanks)
     D xxcmdln         s             15  5 inz(*zeros)
     D #UPCO#          S                   LIKE(UPCO#)
     D #UPOUTQ         S                   LIKE(UPOUTQ)

     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('UDS IMPORT          ')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     C*
     C*================================================================
     C/COPY SCOPYMT01C
     C*================================================================
     C*  PLIST definitions
     C*================================================================
      *
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    ##DIV             5 0
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
      *
     C*  VALIDATE OF GROUP (GROUP IS REQUIRED FIELD)....
      *
     C                   IF        *IN90 <> '1'
     C                   IF        SCFND = *ZEROS
     C                   SETON                                        217490
     C                   ENDIF
     C                   ENDIF
      *
      *  GROUP MUST BE VALID.....
      *
     C                   IF        *IN90 <> '1'
     C                   MOVE      SCFND         ##FND
     C     FNKY          CHAIN     PFN01                              60
     C                   IF        *IN60 = '1'
     C                   SETON                                        217390
     C                   ENDIF
     C                   ENDIF
      *
      * Employer number must be required.......
      *
     C                   IF        *IN90 <> '1'
     C                   IF        SCEMP# = *ZEROS
     C                   SETON                                        217290
     C                   ENDIF
     C                   ENDIF
      *
     C*
     C*  VALIDATE EMPLOYER..........
     C*
     C                   MOVEL     *ZEROS        ##DIV
     C*
     C  N90EMKY          CHAIN     PEM01                              60
     C  N90
     CAN 60              SETON                                        71  90
     C*
     C     *IN90         IFEQ      *OFF

      ************************************************************
      * Call SA039R to process the location with the proper suffix
      * for the data center.
      ************************************************************
      *
     C                   CALL      'SA039R'
     C                   PARM      FOLDER        IFSOBJ1          80
     C                   EVAL      IFSOBJ = %TRIM(IFSOBJ1)
      *
     C                   CALL      'SA039R'
     C                   PARM      FOLDERAR      IFSOBJAR1        80
     C                   EVAL      IFSOBJAR = %TRIM(IFSOBJAR1)
      *
     C                   EVAL      FULLFOLDER = IFSOBJ
      *
      * Check for the existence of the IFS folder "/UDS" (not in QDLS).
      * If it does not exist, create it.
      *
     C                   CALL      'CHKIFSOBJ'
     C                   PARM                    IFSOBJ
     C                   PARM                    IFSOBJFND
      *
     C                   IF        NOT IFSOBJFND
      *
     C                   eval      XXCMD= 'MKDIR DIR(''' +%TRIM(IFSOBJ)+ ''')'
     C                              + ' DTAAUT(*INDIR)'
     C                              + ' OBJAUT(*INDIR) CRTOBJAUD(*SYSVAL)'
     C                              + ' CRTOBJSCAN(*PARENT) RSTDRNMUNL(*NO)'
      *
     C                   eval      XXCMDLN = %LEN(XXCMD)
      *
     C                   Call      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    XXCMDLN
      *
     C                   ENDIF
      *
      * Check for the existence of the IFS folder "/UDS/ARCHIVE" (not
      * in QDLS).  If it does not exist, create it.
      *
     C                   CALL      'CHKIFSOBJ'
     C                   PARM                    IFSOBJAR
     C                   PARM                    IFSOBJFND
      *
     C                   IF        NOT IFSOBJFND
      *
     C                   eval      XXCMD= 'MKDIR DIR(''' +%TRIM(IFSOBJAR)+ ''')'
     C                              + ' DTAAUT(*INDIR)'
     C                              + ' OBJAUT(*INDIR) CRTOBJAUD(*SYSVAL)'
     C                              + ' CRTOBJSCAN(*PARENT) RSTDRNMUNL(*NO)'
      *
     C                   eval      XXCMDLN = %LEN(XXCMD)
      *
     C                   Call      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM                    XXCMDLN
      *
     C                   ENDIF
      *
      * Check for existence of the UDS file "UDSA.TXT" (A format) at
      * the appropriate IFS location (/UDS)
      *
     C                   EVAL      IFSOBJ = %TRIM(PREFIX) +'/'+  %TRIM(FOLDER)
     C                                      + '/' + 'UDSA.TXT'
      *
     C                   Call      'CHKIFSOBJ'
     C                   PARM                    IFSOBJ
     C                   PARM                    IFSOBJFND
      *
     C                   IF        NOT IFSOBJFND
     C                   MOVE      'N'           FLG1              1
     C                   ELSE
     C                   MOVE      'Y'           FLG1
     C                   ENDIF
      *
      * Check for existence of the UDS file "UDSE.TXT" (E format) at
      * the appropriate IFS location (/UDS)
      *
     C                   EVAL      IFSOBJ = %TRIM(PREFIX) +'/'+  %TRIM(FOLDER)
     C                                      + '/' + 'UDSE.TXT'
      *
     C                   Call      'CHKIFSOBJ'
     C                   PARM                    IFSOBJ
     C                   PARM                    IFSOBJFND
      *
     C                   IF        NOT IFSOBJFND
     C                   MOVE      'N'           FLG2              1
     C                   ELSE
     C                   MOVE      'Y'           FLG2
     C                   ENDIF
      *
      *
      * Check for existence of the UDS file "UDSF.TXT" (E format) at
      * the appropriate IFS location (/UDS)
      *
     C                   EVAL      IFSOBJ = %TRIM(PREFIX) +'/'+  %TRIM(FOLDER)
     C                                      + '/' + 'UDSF.TXT'
      *
     C                   Call      'CHKIFSOBJ'
     C                   PARM                    IFSOBJ
     C                   PARM                    IFSOBJFND
      *
     C                   IF        NOT IFSOBJFND
     C                   MOVE      'N'           FLG3              1
     C                   ELSE
     C                   MOVE      'Y'           FLG3
     C                   ENDIF
      *
     C                   IF        FLG1 = 'N' AND FLG2 = 'N' AND FLG3 = 'N'
     C                   SETON                                        229075
     C                   ENDIF
      *
     C                   IF        *IN90 <> '1'
     C                   IF        FLG1 = 'N' AND FLG2 = 'N'
     C                   SETON                                        229076
     C                   ENDIF
     C                   ENDIF

      *  Retrieve the company # (UPCO#) and outq (UPOUTQ) from the
      * users user parameter file WMUMP to pass to other programs.
     C     Q1USER        CHAIN     PUP01
     C                   IF        %FOUND
     C                   MOVE      UPCO#         #UPCO#
     C                   MOVE      UPOUTQ        #UPOUTQ
     C                   ENDIF
      *
      *All the above edits have passed, call W9120C1 to copy the files
      * interactively and submit the job.
      *
     C                   IF        *IN90 <> '1'
      *
     C                   CALL      'W9120C1'
     C                   PARM                    SCFND
     C                   PARM                    SCEMP#
     C                   PARM                    #UPCO#
     C                   PARM                    #UPOUTQ
     C                   PARM                    FLG1
     C                   PARM                    FLG2
     C                   PARM                    FLG3
      *
     C                   ENDIF
      *
      ************************************************************
     C                   ENDIF                                                  *IN90 = '0'
      ************************************************************
     C     *IN90         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN90
      *
     C     *INKC         WHENEQ    *OFF
     C                   CLEAR                   SCFND
     C                   CLEAR                   SCEMP#
      *
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
      *
      * Read file to populate the RRN number (DBRRNF)
      *
     C                   IF        SCFND <> *ZEROS
     C                   MOVEL     SCFND         ##FND
     C     FNKY          CHAIN     PFN01                              60
     C                   ENDIF
      *
      *
     C* SEARCH OF GROUP..
      *
     C                   IF        HFLD =  'SCFND'
      *
     C                   MOVE      SCFND         #@FND
     C                   MOVE      ##CO#         #@CO#
     C*
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    #@CO#             3
     C                   PARM      *BLANKS       #@FND             3
     C                   IF        #@FND <> *BLANKS
     C                   MOVE      #@FND         SCFND
     C                   SETON                                        20
     C                   ELSE
     C                   SETON                                        22
     C                   ENDIF
     C                   ENDIF
     C*
     C*    WORK WITH EMPLOYERS
     C*
     C                   IF        HFLD =  'SCEMP#'
     C*
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRNF        ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN(N)  WMEMP                              60
     C*
     C                   Z-ADD     EMEMP#        SCEMP#
     C*
     C                   ELSE                                                   ##RRN<>*ZEROS
     C*
     C                   SETON                                        20
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   EVAL      *INLR = '1'
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *INKC         @@KC              1
     C                   MOVE      *INKL         @@KL              1
     C                   MOVE      ' '           @@MODE            1
      *
     C                   Z-ADD     *ZEROS        @@RRN             9 0
     C*
     C                   CLEAR                   DBRRNF
     C                   CLEAR                   SCFND
     C                   CLEAR                   SCEMP#
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR                                                  PREOTH
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMEMP
     C*
     C                   ENDSR                                                  WRITER
