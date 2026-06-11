     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     *========================================================================*
    H/TITLE SC092R02 - KEY TYPE MAINTENANCE
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to add, change or view records in the Key Type        *
     *  Definition file.                                                      *
     *                                                                        *
     * CALLED BY : SC092R - WORK WITH KEY TYPES                               *
     *                                                                        *
     * CALLS  TO : NONE                                                       *
     *                                                                        *
     * CREATED   : 01/25/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * 09/13/2000 C:128 PROJECT: 81WRK                                        *
     *            External application auto folder creation.                  *
     *            Added the field SCSRCA to the display. This field will   XX *
     *            contain the name of a program to use to format the          *
     *            folder structure of external applications.                  *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * FILE USAGE:                                                            *
     *                                                                        *
     *  File    Description                    Rcdfmt  Key                    *
     *  SMKTP   DARTS Key Type File            DBRCDF  *Arrival               *
     *  SSYLF   System Control Logical         SDR01   'SC'                   *
     *                                                                        *
     *========================================================================*
     * INDICATOR USAGE:                                                       *
     *  Global Indicators                                                     *
     *   60 - General chain.                                                  *
     *   90 - General error.                                                  *
     *   98 - Image product installed.                                        *
     *                                                                        *
     *  Error Indicators                                                      *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     FSC092F02  CF   E             WorkStn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMKTP     UF A E             Disk    Rename(PKT01:DBRCDF)
     FSSYLF     IF   E           K Disk

     *========================================================================*
     * Procedure Prototypes
     *========================================================================*
      /COPY SC092RINC
      /COPY PSTRINC

     *========================================================================*
     *  /COPY required constants
     *========================================================================*
     D optnm           S             40a
     D comapi          c                   'SA005R'
     D #ofscr          c                   1
     D SC              C                   'SC'

     *========================================================================*
     * Stand Alone Fields - TOP
     *========================================================================*
     D TITLES          s             40    dim(2) ctdata perrcd(1)
     D ##co#           s              3  0
     D ##conm          s             40

     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@view          s              1

     D i               S              3  0
     D temp_1024       S           1024a
    
      /COPY SCOPYMT01C
    
     *========================================================================*
     *  PLIST definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm      *inkc         @@kc
     C                   Parm      *inkl         @@kl
     C                   Parm                    @@mode
     C                   Parm                    @@rrn
     C                   Parm                    @@view
    
     ** Company file/fund (group) retrieve program parameter list
     C     compl         PList
     C                   Parm                    ##co#
     C                   Parm                    ##conm

     *========================================================================*
     *  KLIST definitions
     *========================================================================*

     *========================================================================*
     * DLTPRE - Pre-processing before delete occurs
     *========================================================================*
     C     dltpre        BEGSR
     C                   ENDSR
     *========================================================================*
     * DSPLYR - Processing after displaying information
     *========================================================================*
     C     dsplyr        BEGSR
     C                   ENDSR
     *========================================================================*
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *========================================================================*
     C     errck1        BEGSR

      ** Key type cannot be blank
     C  n90              If        SCKTYP = *blanks
     C                   SetOn                                        71  90
     C                   EndIf

      ** Key program must be valid.
     C  n90              If        SCPNAM = *blanks
     C                   SetOn                                        72  90
     C                   EndIf

      ** Key description cannot be blank.
     C  n90              If        SCPDES = *blanks
     C                   SetOn                                        73  90
     C                   EndIf
      *** Add 09/13/00 *****************************************************
      ** Search program 10 must be valid if present.
     C  n90              If        SCSRCA <> *blanks
     C                   If        ValProgram(SCSRCA) = *on
     C                   SetOn                                        82  90
     C                   EndIf
     C                   EndIf
      *** End 09/13/00 *****************************************************

      ** Error check Image values.
     C                   If        *in98 = *on

      ** Validation program must be valid if present.
     C  n90              If        SCVPGM <> *blanks
     C                   If        ValProgram(SCVPGM) = *on
     C                   SetOn                                        74  90
     C                   EndIf
     C                   EndIf

      ** Index program must be valid if present.
     C  n90              If        SCINPG <> *blanks
     C                   If        ValProgram(SCINPG) = *on
     C                   SetOn                                        75  90
     C                   EndIf
     C                   EndIf

      ** Index description cannot be blank if Index program is present.
     C  n90              If        SCINPG <> *blanks
     C                             AND SCDESC = *blanks
     C                   SetOn                                        76  90
     C                   EndIf

      ** Split program must be valid if present.
     C  n90              If        SCSPLT <> *blanks
     C                   If        ValProgram(SCSPLT) = *on
     C                   SetOn                                        77  90
     C                   EndIf
     C                   EndIf

     C                   EndIf                                                  *in98 = *on

      ** Error check VDARTS values.
     C                   If        *in97 = *on

      ** Search program 1 must be valid if present.
     C  n90              If        SCSRC1 <> *blanks
     C                   If        ValProgram(SCSRC1) = *on
     C                   SetOn                                        78  90
     C                   EndIf
     C                   EndIf

      ** Search program 2 must be valid if present.
     C  n90              If        SCSRC2 <> *blanks
     C                   If        ValProgram(SCSRC2) = *on
     C                   SetOn                                        79  90
     C                   EndIf
     C                   EndIf

      ** Search program 3 must be valid if present.
     C  n90              If        SCSRC3 <> *blanks
     C                   If        ValProgram(SCSRC3) = *on
     C                   SetOn                                        80  90
     C                   EndIf
     C                   EndIf

      ** Search program 3 must be valid if present.
     C  n90              If        SCSRC4 <> *blanks
     C                   If        ValProgram(SCSRC4) = *on
     C                   SetOn                                        81  90
     C                   EndIf
     C                   EndIf


     C                   EndIf                                                  *in97 = *on

     C                   ENDSR
     *========================================================================*
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *========================================================================*
     C     errck2        BEGSR
     C                   ENDSR
     *========================================================================*
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *========================================================================*
     C     errck3        BEGSR
     C                   ENDSR
     *========================================================================*
     * MVDBSC - Move data base fields to screen fields
     *========================================================================*
     C     mvdbsc        BEGSR

     C                   Eval      SCKTYP = KTKTYP
     C                   Eval      SCPNAM = KTPNAM
     C                   Eval      SCPDES = KTPDES
     C                   Eval      SCSTAT = KTSTAT
      *** Add 09/13/00 *****************************************************
     C                   Eval      SCSRCA = KTSRCA
      *** End 09/13/00 *****************************************************

     C   98              Eval      SCVPGM = KTVPGM
     C   98              Eval      SCINPG = KTINPG
     C   98              Eval      SCSPLT = KTSPLT
     C   98              Eval      SCDESC = KTDESC

     C   97              Eval      SCSRC1 = KTSRC1
     C   97              Eval      SCSRC2 = KTSRC2
     C   97              Eval      SCSRC3 = KTSRC3
     C   97              Eval      SCSRC4 = KTSRC4

     C                   ENDSR
     *========================================================================*
     * MVSCDB - Move screen fields to data base fields
     *========================================================================*
     C     mvscdb        BEGSR

     C                   Eval      KTKTYP = SCKTYP
     C                   Eval      KTPNAM = SCPNAM
     C                   Eval      KTPDES = SCPDES
     C                   Eval      KTSTAT = SCSTAT
      *** Add 09/13/00 *****************************************************
     C                   Eval      KTSRCA = SCSRCA
      *** End 09/13/00 *****************************************************

     C   98              Eval      KTVPGM = SCVPGM
     C   98              Eval      KTINPG = SCINPG
     C   98              Eval      KTDESC = SCDESC
     C   98              Eval      KTSPLT = SCSPLT

     C   97              Eval      KTSRC1 = SCSRC1
     C   97              Eval      KTSRC2 = SCSRC2
     C   97              Eval      KTSRC3 = SCSRC3
     C   97              Eval      KTSRC4 = SCSRC4

     C                   If        @@Mode = 'A'
     C                   Eval      KTIPGM = q1pgm
     C                   Eval      KTIUSR = q1user
     C                   Eval      date_in = CvtDate(dsDate : BLNKS : dt_CYMD)
     C                   MoveL     date_in       KTIDT
     C                   Else
     C                   Eval      KTPGM = q1pgm
     C                   Eval      KTUSER = q1user
     C                   Eval      date_in = CvtDate(dsDate : BLNKS : dt_CYMD)
     C                   MoveL     date_in       KTDT
     C                   time                    KTTIME
     C                   EndIf

     C                   ENDSR
     *========================================================================*
     * OTHERR - Processing for other modes
     *========================================================================*
     C     otherr        BEGSR
     C                   ENDSR
     *========================================================================*
     * OTHFKS - Processing for other functions keys
     *========================================================================*
     C     othfks        BEGSR
     C                   ENDSR
     *========================================================================*
     * PGMEND - End of program processing
     *========================================================================*
     C     pgmend        BEGSR
     C                   ENDSR
     *========================================================================*
     * PGMSTR - Start of program processing
     *========================================================================*
     C     pgmstr        BEGSR

      ** Check for the installation of the Image product and set flag.
     C     SC            Chain     SDR01                              60
     C                   If        DRFL01 = yes
     C                   Eval      *in98 = *on
     C                   Else
     C                   Eval      *in98 = *off
     C                   EndIf

      ** Check for the installation of the VDARTS product and set flag.
     C                   If        DRFL03 = yes
     C                   Eval      *in97 = *on
     C                   Else
     C                   Eval      *in97 = *off
     C                   EndIf

     C                   Move      @@View        i
     C*                  Eval      ##data = titles(i)

     C*****              Eval      SCPRDS = Center(titles(i))
     C                   Eval      SCPRDS = titles(i)
     C*                  MoveL     temp_1024     SCPRDS

     C                   ENDSR
     *========================================================================*
     * PREOTH - Pre-processing for other modes
     *========================================================================*
     C     preoth        BEGSR
     C                   ENDSR
     *========================================================================*
     * UPDATR - Update data base record and after processing
     *========================================================================*
     C     updatr        BEGSR

     C     @@rrn         Chain     DBRCDF                             99
B01  C                   If        *in99 = *off
     C                   EXSR      mvscdb
     C                   Update    DBRCDF
E01  C                   EndIf

     C                   ENDSR
     *========================================================================*
     * WRITER - Write data base record and after processing
     *========================================================================*
     C     writer        BEGSR

     C                   Clear                   DBRCDF
     C                   EXSR      mvscdb
     C                   Write     DBRCDF
     C                   FEOD      smktp

     C                   ENDSR
     *========================================================================*
     * ValProgram - Validate a given program.                                 *
     *========================================================================*
      * Description: Converts date formats.                                    *
      *                                                                        *
     * Parameters : 10a - object to check                                     *
      *                                                                        *
      * Returns    :  1a - Error                                               *
     *========================================================================*
      ** Procedure Prototype
     P ValProgram      B

      *** Procedure Interface ***
     D ValProgram      PI             1a
     D  @@Object                     10a

      *** Locals ***
     D xxType          S              7a   Inz('*PGM   ')

      *** Procedure Definition ***
     C                   Eval      xxError = *blank
     C                   CALLP     CheckObj(@@Object:xxType:xxError)
     C                   If        xxError = '0'
     C                   Return    '0'
     C                   Else
     C                   Return    '1'
     C                   EndIf

     P ValProgram      E
     *========================================================================*
** CTDTA TITLES
Key Type Maintenance
Index Maintenance
