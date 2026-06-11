     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     *=====================================================================
     *=====================================================================
     H/TITLE ISIM02R - WC GROUP    IMPORT PROGRAM    TIFF
     *=====================================================================
     * Description:  This program creates a DARTS key to return and updates
     *   the line of business apps database.
     *
     *<!-- This program should be modified to incorporate your specific
     *     database and DARTS key needs.  -->
     *
     * Created   : 07/28/04
     * Programmer: C:02
     * Project   : DRXXX
     *
     *=====================================================================
     * Revisions:
     *
     * 09/16/13 C:33 Project: SY195
     *               Save directory prior to calling IS825R
     *
     * 05/30/08 C:62 PROJECT DR257
     *               ADD ROLLOVER PROCESSING
     *
     * 12/13/05 C:62 PROJECT MC132 50035
     *               PAD FOLDER FIELDS
     *
     * (NOTE: Please add comments to the top of revisions.)
     *=====================================================================
      *
      *
     *=====================================================================
     * File Definitions
     *=====================================================================
     *<!-- Add any files here to update your database -->
      *
     ** Ascent Captured Data File
     *<!-- Change this to your Ascent database file  -->
     FASCREL1   UF   E           K DISK
     *<!-- You must rename your Ascent database record format to PIMP01
     *     for this program to work.  -->
     F                                     RENAME(ASC1:PIMP01)
      *
     *<!-- Change this to your Ascent database cross reference file -->
     ** Ascent Document Location Cross Reference File
     FASCREL2   IF   E           K DISK
     F                                     RENAME(ASC2:PIMP02)
      *
     *=====================================================================
     * Constant definitions
     *=====================================================================
     ** Program control

     D TRUE            C                   CONST('1')
     D FALSE           C                   CONST('0')
      *
     ** Infinity Image API's
     D CPYDLO          C                   CONST('IS801C')
     D DLTDLO          C                   CONST('IS802C')
     D UPBCHS          C                   CONST('IS820R')
     D GTDCTP          C                   CONST('IS822R')
     D GTDCIX          C                   CONST('IS823R')
     D UPDOCI          C                   CONST('IS824R')
     D GTDOCN          C                   CONST('IS825R')
     D INCIDX          C                   CONST('IS826R')
      *
     ** PROGRAM TO PROCESS ADDTIONAL INFORMATION
     D EXTPGM          C                   CONST('ISIM02R1')
      *
     ** Status
     D STSCMP          C                   CONST('C')
     D STSERR          C                   CONST('E')
     D STSIDX          C                   CONST('I')
     D STSQUE          C                   CONST('Q')
      *
      ***  Begin Add  ***  09/16/13  *************************************
     DsaveIMdDir       s                   like(IMdDir)
      ***  End   Add  ***  09/16/13  *************************************
      *
     *=====================================================================
     * Data Structure, Table & Array definitions
     *=====================================================================
     ** Program information
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D IMPDTA        E DS                  EXTNAME(ASCREL1)
      *
     ** Date/Time
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
     *=====================================================================
     * PList definitions
     *=====================================================================
     *<!-- This plist MUST stay the same to avoid disaster! -->
     * Entry parms

     C     *ENTRY        PLIST
     C                   PARM                    @WKSID            2            Work Station Id
     C                   PARM                    @DCTYP            8            Doc Type
     C                   PARM                    @RTNCD            1            Return Code
      *
     *** Infinity Image API parms ***
      *
     ** Copy DLO parms
     C     PCPDLO        PLIST
     C                   PARM                    IMDCID
     C                   PARM                    IMDDIR
     C                   PARM                    BTDCID
     C                   PARM                    NEWDCF           80
     C                   PARM      *BLANKS       RTNCOD
      *
     ** Delete DLO parms
     C     PDTDLO        PLIST
     C                   PARM                    IMDCID
     C                   PARM                    IMDDIR
     C                   PARM      *BLANKS       RTNCOD
      *
     ** Increment Index parms
     C     PINCDX        PLIST
     C                   PARM                    @WKSID                         Work Station Id
     C                   PARM      '0'           FLDOC             1
     C                   PARM      '1'           FLBCH             1
     C                   PARM      *BLANKS       RTNCOD
      *
     ** Get Doc Type Info parms
     C     PGTDCT        PLIST
     C                   PARM                    @DCTYP
     C                   PARM                    RMKCD             5
     C                   PARM                    FILSYS           10
     C                   PARM                    DELIM             3
     C                   PARM                    DOCDSC           40
     C                   PARM                    IDXQUE           10
     C                   PARM                    #DCBCH            5
     C                   PARM                    SECLVL            3
     C                   PARM                    IDXPGM           10
     C                   PARM                    BCHSTS            2
     C                   PARM                    IMPPGM           10
     C                   PARM                    IMPDIR           12
     C                   PARM      *BLANKS       RTNCOD            1
      *
     ** Get Doc Index Info parms
     C     PGTDCI        PLIST
     C                   PARM                    @WKSID
     C                   PARM                    DOCSEQ            6
     C                   PARM                    SUFFIX            3
     C                   PARM                    BCHSEQ            4
     C                   PARM                    BTDCID           12
     C                   PARM                    BTBCHI           12
     C                   PARM                    IMGFLD            8
     C                   PARM                    IMGSFL           12
     C                   PARM                    NDOCFL           21
     C                   PARM      *BLANKS       RTNCOD
      *
     ** Update Batch Record parms
     C     PUPBCH        PLIST
     C                   PARM                    BTBCHI
     C                   PARM                    PBCHST            2
     C                   PARM                    @DCTYP
     C                   PARM      Q1USER        SCNREP           10
     C                   PARM                    SCDATE            8
     C                   PARM                    SCTIME            6
     C                   PARM                    E#DOCS            5
     C                   PARM                    A#DOCS            5
     C                   PARM                    E#PAGE            5
     C                   PARM                    A#PAGE            5
     C                   PARM                    #PGPDC            3
     C                   PARM      *BLANK        DOCSFL            1
     C                   PARM                    Q1PGM
     C                   PARM      *BLANKS       BCERDS           52
     C                   PARM      DOCDSC        BCHDSC           40
     C                   PARM      *BLANKS       RTNCOD
      *
     ** Update Doc Info parms
     C     PUPDCI        PLIST
     C                   PARM                    @WKSID
     C                   PARM                    DOCSEQ
     C                   PARM                    DBCHSQ            4
     C                   PARM                    IMGFLD
     C                   PARM                    IMGSFL
     C                   PARM                    A#DOCS
     C                   PARM                    @DCTYP
     C                   PARM                    BTDCID
     C                   PARM                    #BYTES            5
     C                   PARM                    SCDATE
     C                   PARM                    SCTIME
     C                   PARM                    BTBCHI
     C                   PARM                    #PGTDC            4
     C                   PARM                    DARTKY           40
     C                   PARM      '00000'       DARTSQ            5
     C                   PARM      ' '           RTNCOD
     *** ABOVE RTNCOD IS BLANK NORMALLY... NDP PROCESSING REQUIRED A
     *** "Y" FOR SPECIAL SITUATION (DUMMY KEY ALLOWED)
     C*
      *
     ** Get Doc Name & Path parms
     C     PGTDCN        PLIST
     C                   PARM                    ASDPTH
     C                   PARM                    IMDDIR
     C                   PARM                    IMDCID
      *
      *
     ** EXT PGM PARMS
     C     EXTDTA        PLIST
     C                   PARM                    IMPDTA
     C                   PARM                    RTNCO             7
     C                   PARM                    AFIELD           40
      *
     *=====================================================================
     * KList definitions
     *=====================================================================
     C     IMPKEY        KLIST
     C                   KFLD                    @DCTYP
      *
      *
     *=====================================================================
     * Main
     *=====================================================================
     ** Initalization
     C                   EXSR      SRINIT
      *
     ** Calculate the estimated number of documents and pages
     C                   EXSR      NUMDOC
      *
     ** Get record from the database
     C     IMPKEY        CHAIN     PIMP01                             60
      *
     ** Do while non-qued and non-indexed entries exist
     C     *IN60         DOWEQ     *OFF
     C     ANUMDC        ANDLT     MAXDOC
      *
     ** Only do if the record has not been indexed or queued
     C     IMSTAT        IFEQ      ' '
      *
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SCDATE
     C                   MOVE      DSTIME        SCTIME
      *
     ** If this is the first entry, write a batch record
     C     FIRST         IFEQ      TRUE
     C                   EXSR      SRFRST
     C                   ENDIF                                                  FIRST = TRUE
      *
     C                   MOVEL     IMDDIR        OLDDIR           80
      *
     ** Perform the user specific processing
     *<!-- This is where your subroutine is called (define below) -->
     C                   EXSR      IMPORT
      *
     ** Process the document
     C                   MOVEL     *BLANKS       RTNCOD
     C                   EXSR      PRCDOC
      *
     ** Update the Ascent record with 'Q', 'I' or 'E'
     C** IF NO ERRORS FROM PRCDOC THEN DO SR.
     C     RTNCOD        IFEQ      *BLANK
     C                   EXSR      UPDDBF
     C                   ENDIF                                                  RTNCOD = ' '
      *
     C                   ENDIF                                                  IMSTAT = ' '
      *
     ** Get the next record
     C     IMPKEY        READE     PIMP01                                 60
     C                   ENDDO
      *
     C                   EXSR      FINISH
      *
     ** End the program
     C                   MOVE      *ON           *INLR
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * SRINIT - Initialization subroutine
     *=====================================================================
     C     SRINIT        BEGSR
      *
     ** Initialize the date/time data structure
     C                   TIME                    DSDTTM
      *
     ** Get Doc type info
     C                   CALL      GTDCTP        PGTDCT
     C                   MOVE      DELIM         NUMPGD            3 0
     C                   MOVE      NUMPGD        #PGPDC
     C                   MOVE      NUMPGD        #PGTDC
     C                   MOVE      #DCBCH        MAXDOC            5 0
     C     MAXDOC        IFEQ      0
     C                   MOVE      *HIVAL        MAXDOC
     C                   SUB       1             MAXDOC
     C                   ENDIF
      *
     ** Get Doc index info
     C                   CALL      GTDCIX        PGTDCI
     C*** BEGIN ADD *** 05/30/08 **************************************
     C*
     C* IF THE PROGRAM ENDED ABNORMALLY DUE TO ROLLOVER ERROR THEN
     C* PASS THAT ERROR CODE TO IS800R
     C*
     C     RTNCOD        IFEQ      'S'
     C                   MOVEL(P)  RTNCOD        @RTNCD
     C                   RETURN
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVE      BCHSEQ        DBCHSQ
     C                   MOVE      BCHSTS        SVBSTS            2
      *
     ** Initialize control flags
     C                   MOVE      TRUE          FIRST
     C                   MOVE      FALSE         MADBCH
      *
     C                   ENDSR                                                  SRINIT
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * NUMDOC - Calculate the estimated number of documents
     *=====================================================================
     C     NUMDOC        BEGSR
      *
     C     IMPKEY        CHAIN     PIMP01                             60
      *
     ** Do while entries exist
     C     *IN60         DOWEQ     *OFF
     C     ENUMDC        ANDLT     MAXDOC
      *
     ** Only count if the record has not been indexed or queued
     C     IMSTAT        IFEQ      ' '
      *
     ** Increment total number of docs processed
     C                   ADD       1             ENUMDC            5 0
      *
     ** Increment total number of pages
     C                   ADD       NUMPGD        ENUMPG            5 0
      *
     C                   ENDIF                                                  IMSTAT = ' '
      *
     C     IMPKEY        READE     PIMP01                                 60
     C                   ENDDO
      *
     C                   MOVE      ENUMDC        E#DOCS
     C                   MOVE      ENUMPG        E#PAGE
      *
     C                   ENDSR                                                  NUMDOC
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * PRCDOC - Process the document
     *=====================================================================
     C     PRCDOC        BEGSR
      *
     ** Get the document path from the cross reference file
     C     IMASDI        CHAIN     PIMP02                             60
      *
     ** Get the name of the document and the path from the string
      /Free
       //***  Begin Add  ***  09/16/13  ***********************************
       // **  save IMDDIR and use it post GTDOCN call we NEED the DCID extracted
       saveIMdDir = IMdDir;
       //***  End   Add  ***  09/16/13  ***********************************
      /End-free
     C                   CALL      GTDOCN        PGTDCN
      /Free
       //***  Begin Add  ***  09/16/13  ***********************************
       //** replace the returned DDIR value with the saved one to exclude the docroot
       IMdDir = saveIMdDir;
       //***  End   Add  ***  09/16/13  ***********************************
      /End-free
      *
     ** Copy the doc to a built name/folder
     C*** BEGIN CHG *** 12/13/05 **************************************
     C**********           MOVELNDOCFL    NEWDCF
     C                   MOVEL(P)  NDOCFL        NEWDCF
     C*** END   CHG *** 12/13/05 **************************************
     C                   CALL      CPYDLO        PCPDLO
     C*** CLEARINFO FOR ASCNET DB UPDATE ****
     C                   MOVEL     *BLANKS       #@DOC            12
     C                   MOVEL     *BLANKS       #@FLR            80
     C**********************************************************
     C     RTNCOD        CABNE     *BLANK        NOGO1
     C*** SAVE INFO FOR ASCNET DB UPDATE ****
     C                   MOVEL     BTDCID        #@DOC            12
     C*** BEGIN CHG *** 12/13/05 **************************************
     C**********           MOVELNEWDCF    #@FLR  80
     C                   MOVEL(P)  NEWDCF        #@FLR            80
     C*** END   CHG *** 12/13/05 **************************************
     C**********************************************************
      *
      *
     ** Update the document info record
     C                   CALL      UPDOCI        PUPDCI
      *
     ** Get Doc index info
     C                   CALL      GTDCIX        PGTDCI
     C*** BEGIN ADD *** 05/30/08 **************************************
     C*
     C* IF THE PROGRAM ENDED ABNORMALLY DUE TO ROLLOVER ERROR THEN
     C* PASS THAT ERROR CODE TO IS800R
     C*
     C     RTNCOD        IFEQ      'S'
     C                   MOVEL(P)  RTNCOD        @RTNCD
     C                   RETURN
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
      *
     ** Delete the original document
     C                   CALL      DLTDLO        PDTDLO
      *
     ** Increment total number of docs processed
     C                   ADD       1             ANUMDC            5 0
      *
     ** Increment total number of pages
     C                   ADD       NUMPGD        ANUMPG            5 0
      *
     C***
     C     NOGO1         TAG
     C                   ENDSR                                                  PRCDOC
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * SRFRST - Processing the first time around
     *=====================================================================
     C     SRFRST        BEGSR
      *
     C                   MOVE      FALSE         FIRST             1
     C                   MOVE      TRUE          MADBCH            1
     C                   MOVE      'SC'          PBCHST
      *
     ** Create the batch record
     C                   CALL      UPBCHS        PUPBCH
      *
     C                   ENDSR                                                  SRFRST
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * UPDDBF - Update the Ascent database file with the status
     *=====================================================================
     C     UPDDBF        BEGSR
      *
     C     DARTKY        IFEQ      *BLANKS
     C                   MOVE      STSQUE        IMSTAT
     C                   ELSE
     C                   MOVE      STSIDX        IMSTAT
     C                   ENDIF
      *
      *
     C                   MOVEL     #@DOC         DRIDOC
     C*****      IMGFLD    CAT  '/':0     DRFOLD    P
     C********             CAT  IMGSFL:0  DRFOLD    P
     C*** BEGIN CHG *** 12/13/05 **************************************
     C**********           MOVEL#@FLR     DRFOLD
     C                   MOVEL(P)  #@FLR         DRFOLD
     C*** END   CHG *** 12/13/05 **************************************
     C                   MOVE      DARTSQ        DRSEQ#
      *
     C                   UPDATE    PIMP01
      *
     C                   ENDSR                                                  UPDDBF
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * FINISH - DO clean up stuff
     *=====================================================================
     C     FINISH        BEGSR
      *
     ** If a batch was made, do stuff
     C     MADBCH        IFEQ      TRUE
      *
     C                   MOVE      ANUMDC        A#DOCS
     C                   MOVE      ANUMPG        A#PAGE
     C                   MOVE      ENUMDC        E#DOCS
     C                   MOVE      ENUMPG        E#PAGE
      *
     ** Update the batch status record
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SCDATE
     C                   MOVE      DSTIME        SCTIME
     C                   MOVE      SVBSTS        PBCHST
     C                   CALL      UPBCHS        PUPBCH
      *
     ** Increment the index info
     C                   CALL      INCIDX        PINCDX
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  FINISH
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * IMPORT - Do import stuff
     *<!-- This subroutine is where you insert your code to create a
     *     DARTS index key and update your database files -->
     *=====================================================================
     C     IMPORT        BEGSR
      *
     *<!-- You must move the created DARTS key to this field if you want
     *     your document indexed. -->
      *
      *** CALL THE EXTPGM TO PROCESS THE REQUEST AND RETURN THE KEY TO
      *** TO WRITE (INDEX) THE APP
      *
     C                   MOVE      *BLANKS       RTNCO
     C                   MOVE      *BLANKS       AFIELD
      *
      * * * ONLY PROCESS IF INCOMING DART KEY IS NOT ALREADY *BLANKS
      *
     C     IMDKEY        IFNE      *BLANKS
      *
     C                   CALL      EXTPGM        EXTDTA
      *
     C     RTNCO         IFEQ      *BLANKS
     C                   MOVEL     AFIELD        DARTKY
     C                   MOVEL     'Y'           INDEXD
     C                   ELSE
     C                   MOVEL     *BLANKS       DARTKY
     C                   MOVEL     *BLANKS       INDEXD
     C                   ENDIF
      *
     C                   ENDIF
      *
     *** FILL BATCH INFO TO RECORDS PROCESSED REGARDLESS OF INDEXED OR NOT
     C                   MOVE      BTBCHI        ISBCHI
     C                   MOVE      SCDATE        IMSDAT
     C                   MOVE      SCTIME        IMSTIM
     C                   MOVEL     Q1USER        IMSUSR
      *
     C                   ENDSR                                                  IMPORT
     *=====================================================================
