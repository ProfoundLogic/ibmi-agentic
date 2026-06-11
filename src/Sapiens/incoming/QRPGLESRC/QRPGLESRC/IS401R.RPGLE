     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* * *  WRITE Incoming Fax records to IMIFP and SAIFP(pds)
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 10/17/11 C:279 Project: SY195                                *
     H*               - Add CALL to API ISA700R to remove direct     *
     H*                 access to IMDOP.                             *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F* * * SAIFP IS KEPT HERE FOR COMPATIBILITY WITH PDS(C) DARTS
     FSAIFP     O    E             DISK
     F                                     RENAME(PIF01:PIF02)
     FIMIFP     O    E             DISK
     F*
     FIMFPL     IF   E           K DISK
     F*
     F***** BEG  DEL ***  10/17/11  **********************************
     F*****IMDOP   O   E                    DISK
     F***** END  DEL ***  10/17/11  **********************************
     F*
     F*
     F*

     D DN              S             75    DIM(5)                               DOC NOTES
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D                 DS
     D  Q#LFM                  1     30
     D  Q#LIBN                 1     10
     D  Q#DBFN                11     20
     D  Q#MBRN                21     30
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FC6             C                   CONST('                    -
     D                                        ')
     D CMA             C                   CONST('Fax Received from:')
     D CMB             C                   CONST('FaxERRORPartial:')
     D*
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('Sent by:')
     D MID             C                   CONST('&QUEUE  ID')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('** Incoming Fax **')
     D DHBR            C                   CONST('# of Pages:')
     D DTOQ            C                   CONST('to Queue:')

     I*

     C*
     C*
     C*
     C*  TRANSACTION FILE
     C*
     C*
     C*
     C*** PARAMERTER LISTS FOR EXTERNAL CALLS
     C*
     C*   PARM LIST FOR "RSNDFXC" CL PGM
     C*
     C     PLSEND        PLIST
     C                   PARM                    QQPHN#           32
     C*
     C     PLSFXE        PLIST
     C                   PARM                    QQPHN#
     C                   PARM                    QQRTCD            1
     C*
     C*
     C*
     C     FPKEY         KLIST
     C                   KFLD                    ##FAXP           12
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQLIBN           10            SPFL JOB NAME
     C                   PARM                    QQDBFN           10
     C                   PARM                    QQMBRN           10            SPLF JOB NUMBER
     C                   PARM                    QQ#PGS            5            SPLF NAME
     C                   PARM                    QQTSI#           20            SPLF NUMBER
     C                   PARM                    QQSTS             1
     C                   PARM                    QQFXCN            8
     C                   PARM                    QQFXPT            4
     C                   PARM                    QQFDTE            6
     C                   PARM                    QQFTIM            6
     C                   PARM                    QQROUT           10
     C                   PARM                    QQDT8             8
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           63
     C                   PARM                    QQRTCD            1
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C***** BEG  ADD ***  10/17/11  **********************************
     C     ISAKEY        PLIST
     C                   PARM                    QQDCID
     C                   PARM                    QQCIF
     C                   PARM                    QQCISF
     C                   PARM                    QQOSYS            1
     C                   PARM                    QQLBN            10
     C                   PARM                    QQOFLD            8
     C                   PARM                    QQOSFL           12
     C                   PARM                    QQODCI           12
     C                   PARM                    QQARQS            4 0
     C                   PARM                    QQDTYP            8
     C                   PARM                    QQBYTD            5 1
     C                   PARM                    QQWSID            2
     C                   PARM                    QQSDT             8 0
     C                   PARM                    QQSTMD            6 0
     C                   PARM                    QQSREP            8
     C                   PARM                    QQBCHI           12
     C                   PARM                    QQPGS             4 0
     C                   PARM                    QQAIFL            1
     C                   PARM                    QQOPTF            1
     C                   PARM                    QQIDFX            1
     C                   PARM                    QQVOL1           10
     C                   PARM                    QQVOL2           10
     C                   PARM                    QQVOL3           10
     C                   PARM                    QQDKEY           40
     C                   PARM                    QQDSQ#            5 0
     C                   PARM                    QQFL01            1
     C                   PARM                    QQFL02            1
     C                   PARM                    QQFL03            1
     C                   PARM                    QQFL04            1
     C                   PARM                    QQFL05            1
     C                   PARM                    QQCHR5            5
     C                   PARM                    QQDSTS            2
     C                   PARM                    QQRCVD            8 0
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQINDX            1
     C                   PARM                    QQRTRN            1
     C***** END  ADD ***  10/17/11  **********************************
     C*
     C                   MOVEL     QQLIBN        Q#LIBN
     C                   MOVEL     QQDBFN        Q#DBFN
     C                   MOVEL     QQMBRN        Q#MBRN
     C*
     C                   MOVE      QQ#PGS        ###PGS            5 0
     C                   MOVE      QQFDTE        ##FDTE            6 0
     C                   MOVE      QQFTIM        ##FTM             6 0
     C                   MOVE      QQDT8         ##DT8             8 0
     C                   MOVEL     *BLANKS       QQDCID
     C                   MOVEL     *BLANKS       QQFOLD
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   TIME                    DSDTTM
     C*
      *================================================================
     C*
     C*** GET NEXT AVAILABLE DOC ID AND FOLDER FOR SAVING THE FAX...
     C*
     C                   CALL      'IS403R'
     C                   PARM                    QQDCID
     C                   PARM                    QQCIF             8
     C                   PARM                    QQCISF           12
     C                   PARM                    QQRTCD
     C*
     C     QQCIF         IFNE      *BLANKS
     C*
     C     QQCIF         CAT(P)    '/':0         QQFOLD
     C                   CAT(P)    QQCISF:0      QQFOLD
     C*
     C                   ELSE
     C                   MOVEL     'DARTSFAX'    QQFOLD
     C                   ENDIF
     C*
     C*** PUT CODE HERE TO RECOVER FROM ERROR ???????????????????????????
     C*
     C****
     C*
     C*** CALL PGM TO FILE FAX **
     C*
     C*
     C                   CALL      'IS402C'
     C                   PARM                    QQDCID
     C                   PARM                    QQFOLD
     C                   PARM                    QQLIBN
     C                   PARM                    QQDBFN
     C                   PARM                    QQMBRN
     C                   PARM      *BLANK        #TERR             1
     C*
     C*** PUT CODE HERE TO RECOER FROM ERROR ???????????????????????????
      *   * IF THE #TERR FLD RETURNS AS "E" THEN THE 508 ERROR HAS OCC
      *     URD, AND FOR NOW, BYPASS THE FILING OF THIS ITEM.....
      ************************
     C     #TERR         CABEQ     'E'           TERM
     C*
     C*** WRITE DATA BASE RECORD SAIFP AND IMIFP ***
     C*
     C                   MOVEL     QQLIBN        IFLIBN
     C                   MOVEL     QQDBFN        IFDBFN
     C                   MOVEL     QQMBRN        IFMBRN
     C*
     C                   MOVEL     Q#LFM         IFLFM
     C*
     C                   MOVEL     QQ#PGS        IF#PGS
     C                   MOVEL     QQTSI#        IFTSI
     C*
     C                   MOVEL     QQSTS         IFCSTS
     C                   MOVEL     QQFXCN        IFCNAM
     C                   MOVEL     QQFXPT        IFPORT
     C                   MOVEL     QQFDTE        IFRDAT
     C                   MOVEL     QQFTIM        IFRTIM
     C     QQSTS         IFEQ      'S'
     C                   MOVEL     'COMPLETE'    IFSTS
     C                   ELSE
     C                   MOVEL     'PARTIAL '    IFSTS
     C                   ENDIF
     C                   MOVEL     QQDCID        IFDCID
     C                   MOVEL     QQFOLD        IFFOLD
     C                   MOVEL     'A'           IFSTAT
     C                   MOVEL     Q1USER        IFUSER
     C                   MOVEL     Q1PGM         IFPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTCYMD
     C                   MOVEL     SRDTO         IFDT
     C*
     C                   Z-ADD     DSTIME        IFTIME
     C                   MOVEL     Q1USER        IFIUSR
     C*
     C*** CVT FROM MDYYYY (NUM) TO YYYYMMDD (NUMERIC)
     C*
     C                   Z-ADD     ##DT8         IFRDT8
     C*
     C                   MOVE      ##FTM         IFRTM
     C*
      *
     C                   MOVEL     QQROUT        IFROUT
     C***
      *
      * * *  CREATE FAX DESCRIPTION FIELD **** IFDESC
      *
     C     QQSTS         IFEQ      'S'
     C     CMA           CAT(P)    IFTSI:1       IFDESC
     C                   ELSE
     C     CMB           CAT(P)    IFTSI:1       IFDESC
     C                   ENDIF
      *
     C*
     C                   WRITE     PIF01
     C                   WRITE     PIF02
      *
     C*** WRITE THE RECORD TO THE DO FILE NOW*******************
     C*
     C***
     C                   EXSR      WRTDO
     C*
     C*
     C**** Do Routing Routines now!!!
      *
      *
     C*** CALL THE PROGRAM THAT WILL
     C*** DETERMINE WHAT KIND OF ROUTING IS TO BE PERFORMED....THEN IT
     C*** CALL THE NECESSARY PROGRAMS TO PERFORM THE ROUTING.
      *
      * * * SHOULD WE 1ST WRITE THE DO RECORD???  DO WE WRITE AN IE TO
      * * * THE DEFAULT QUEUE 1ST, THEN GO THROUGH ROUTING TO SEE IF WE
      * * * WILL CHANGE IT (NO***) OR *** GO THROUGH THE ROUTING ROUTINES
      * * *
      *
      **** GET THE CONTROL FILE IMFPP HERE AND DETERMINE WHAT ROUTING IS
      * *  PERFORMED BY FOR THIS PORT.
      * *
     C     QQFXCN        CAT       QQFXPT        ##FAXP
     C     FPKEY         CHAIN     PFP01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*** DETERMINE THE ROUTING FLAGS FOR THIS PORT
     C*
      *
      * * * ******************** * * * ***********************************
      * * * SPECIFIC TSI ROUTING * * * ***********************************
      * * * ******************** * * * ***********************************
      *
     C     FPTSIR        IFEQ      'Y'
      *
     C                   CALL      'IS410R1'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQLIBN           10            LIB NAM-FAXMBR
     C                   PARM                    QQDBFN           10            FIL NAM-FAXMBR
     C                   PARM                    QQMBRN           10            MBR NAM-FAXMBR
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSI#           20            TRSCRBR ID (20)
     C                   PARM                    QQSTS             1            COMPLETION STS
     C                   PARM                    QQFXCN            8            FAX CONTROLLER
     C                   PARM                    QQFXPT            4            FAX PORT
     C                   PARM                    QQFDTE            6            RCV DT (6)
     C                   PARM                    QQFTIM            6            RCV TIM
     C                   PARM                    QQROUT           10            FAX ROUT INFO
     C                   PARM                    QQDT8             8            RCV DATE (8)
     C                   PARM                    QQDCID           12            DOC ID FOR .FAX
     C                   PARM                    QQFOLD           63            FOLDER FOR .FAX
     C                   PARM      FPPART        QQPART            1            PERFORM PART LKUP
     C                   PARM      FPPPGM        QQPPGM           10            PARTIAL PGM
     C                   PARM      FPPLIB        QQPLIB           10            PARTIAL PGMLIB
     C                   PARM      FPIMGQ        QQDFTQ           10            DFT QUEUE
     C                   PARM                    $$RTNC            1            RTNCODE ********
      *
     C     $$RTNC        IFEQ      'N'
      * * *  DID NOT DO THE TSID ROUTING.....?????
     C                   ELSE
      * * *  DID ROUTE USING TSI.... BYPASS OTHER ROUTING ROUTINES...
     C*
     C                   GOTO      ENDRTE
     C                   ENDIF
      *
     C                   ENDIF                                                  END TSIR
      *
      * * * ************************ * * *
      * * * END SPECIFIC TSI ROUTING * * *
      * * * ************************** * *
      *
      * * * ********************* * * * **********************************
      * * * FAX PORT LOAD BALANCE * * * **********************************
      * * * ********************* * * * **********************************
     C*** DO THE FAXPORT LOAD BALANCING ROUTINE ****
     C*
      *
     C     FPLBR         IFEQ      'Y'
      *
     C                   CALL      'IS410R2'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQLIBN           10            LIB NAM-FAXMBR
     C                   PARM                    QQDBFN           10            FIL NAM-FAXMBR
     C                   PARM                    QQMBRN           10            MBR NAM-FAXMBR
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSI#           20            TRSCRBR ID (20)
     C                   PARM                    QQSTS             1            COMPLETION STS
     C                   PARM                    QQFXCN            8            FAX CONTROLLER
     C                   PARM                    QQFXPT            4            FAX PORT
     C                   PARM                    QQFDTE            6            RCV DT (6)
     C                   PARM                    QQFTIM            6            RCV TIM
     C                   PARM                    QQROUT           10            FAX ROUT INFO
     C                   PARM                    QQDT8             8            RCV DATE (8)
     C                   PARM                    QQDCID           12            DOC ID FOR .FAX
     C                   PARM                    QQFOLD           63            FOLDER FOR .FAX
     C                   PARM      FPIMGQ        QQDFTQ           10            DFT QUEUE
     C                   PARM      FPDOCT        QQDOCT            8            DOCTYP
     C                   PARM                    $$RTNC            1            RTNCODE ********
      *
     C     $$RTNC        IFEQ      'N'
      * * *  DID NOT DO THE FAX PORT LOAD BALANCE ****
     C                   ELSE
      *
      * * *  DID ROUTE USING FAX PORT LOAD BALANCE ... DONT NEED TO DO MORE
     C*
      *
     C                   GOTO      ENDRTE
     C                   ENDIF
      *
     C                   ENDIF                                                  END LBR
      *
      * * * ************************* * * *
      * * * END FAX PORT LOAD BALANCE * * *
      * * * ************************* * * *
      *
      * * * ************************************ * * * *******************
      * * * DEFAULT QUEUE ASSIGNMENT BY FAX PORT * * * *******************
      * * * ************************************ * * * *******************
     C*** USE THE FAX PORTS DEFAULT QUEUE ID  ****
     C*
     C*
      *
     C     FPIMGQ        IFNE      *BLANKS
      *
     C                   CALL      'IS410R3'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQLIBN           10            LIB NAM-FAXMBR
     C                   PARM                    QQDBFN           10            FIL NAM-FAXMBR
     C                   PARM                    QQMBRN           10            MBR NAM-FAXMBR
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSI#           20            TRSCRBR ID (20)
     C                   PARM                    QQSTS             1            COMPLETION STS
     C                   PARM                    QQFXCN            8            FAX CONTROLLER
     C                   PARM                    QQFXPT            4            FAX PORT
     C                   PARM                    QQFDTE            6            RCV DT (6)
     C                   PARM                    QQFTIM            6            RCV TIM
     C                   PARM                    QQROUT           10            FAX ROUT INFO
     C                   PARM                    QQDT8             8            RCV DATE (8)
     C                   PARM                    QQDCID           12            DOC ID FOR .FAX
     C                   PARM                    QQFOLD           63            FOLDER FOR .FAX
     C                   PARM      FPIMGQ        QQDFTQ           10            DFT QUEUE
     C                   PARM      FPDOCT        QQDOCT            8            DOCTYP
     C                   PARM                    $$RTNC            1            RTNCODE ********
      *
     C     $$RTNC        IFEQ      'N'
      * * *  DID NOT DO THE DEFAULT FAX PORT FAX QUEUE ****
     C                   ELSE
      *
      * * *  DID ROUTE USING FAX PORT DEFAULT FAX QUEUE DONT DO MORE
     C*
      *
     C                   GOTO      ENDRTE
     C                   ENDIF
      *
     C                   ENDIF                                                  END IMGQ
      *
      * * * ************************* * * *
      * * * END FAX PORT DEFAULT QUE  * * *
      * * * ************************* * * *
      *
      *
     C*
     C                   ELSE                                                   FP NOT FOUND
      *
      *
      * * * ******************************** * * * *******************
      * * * DEFAULT QUEUE ASSIGNMENT GENERAL * * * *******************
      * * * ******************************** * * * *******************
      *** USE GENERAL DEFAULT QUEUE FOR FAXES ("FAXDEFAULT")
      *
     C                   CALL      'IS410R4'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQLIBN           10            LIB NAM-FAXMBR
     C                   PARM                    QQDBFN           10            FIL NAM-FAXMBR
     C                   PARM                    QQMBRN           10            MBR NAM-FAXMBR
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSI#           20            TRSCRBR ID (20)
     C                   PARM                    QQSTS             1            COMPLETION STS
     C                   PARM                    QQFXCN            8            FAX CONTROLLER
     C                   PARM                    QQFXPT            4            FAX PORT
     C                   PARM                    QQFDTE            6            RCV DT (6)
     C                   PARM                    QQFTIM            6            RCV TIM
     C                   PARM                    QQROUT           10            FAX ROUT INFO
     C                   PARM                    QQDT8             8            RCV DATE (8)
     C                   PARM                    QQDCID           12            DOC ID FOR .FAX
     C                   PARM                    QQFOLD           63            FOLDER FOR .FAX
     C                   PARM                    QQDFTQ           10            DFT QUEUE
     C                   PARM                    QQDOCT            8            DOCTYP
     C                   PARM                    $$RTNC            1            RTNCODE ********
      *
      *
     C     $$RTNC        IFEQ      'N'
      * * *  DID NOT DO THE DEFAULT FAX PORT FAX QUEUE ****
     C                   ELSE
      *
      * * *  DID ROUTE USING FAX GENERAL DFT FAX QUEUE DONT DO MORE
     C*
      *
     C                   GOTO      ENDRTE
     C                   ENDIF
      *
      *
     C                   ENDIF                                                  FP CHAIN
      *
      * * * ************************* * * *
      * * * END GENERAL FAX QUEUE     * * *
      * * * ************************* * * *
      *
      *
     *** *** *** *** *** *** *** *** ***
     *** END OF FAX ROUTING  *** *** ***
     *** *** *** *** *** *** *** *** ***
      *
     C     ENDRTE        TAG
     C*
     C*** VALIDATE THAT AT LEAST 1 IE EXISTS, IF NOT, CREATE ONE FOR
     C***
     C*** THE DEFAULT FAX STUFF
     C*
     C*
      *
     C                   CALL      'IS410R5'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQLIBN           10            LIB NAM-FAXMBR
     C                   PARM                    QQDBFN           10            FIL NAM-FAXMBR
     C                   PARM                    QQMBRN           10            MBR NAM-FAXMBR
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSI#           20            TRSCRBR ID (20)
     C                   PARM                    QQSTS             1            COMPLETION STS
     C                   PARM                    QQFXCN            8            FAX CONTROLLER
     C                   PARM                    QQFXPT            4            FAX PORT
     C                   PARM                    QQFDTE            6            RCV DT (6)
     C                   PARM                    QQFTIM            6            RCV TIM
     C                   PARM                    QQROUT           10            FAX ROUT INFO
     C                   PARM                    QQDT8             8            RCV DATE (8)
     C                   PARM                    QQDCID           12            DOC ID FOR .FAX
     C                   PARM                    QQFOLD           63            FOLDER FOR .FAX
     C                   PARM                    QQDFTQ           10            DFT QUEUE
     C                   PARM                    QQDOCT            8            DOCTYP
     C                   PARM                    $$RTNC            1            RTNCODE ********
      *
      *
     C*
      *================================================================
     C* END OF THE PGM
     C*
     C     TERM          TAG
     C*
     C                   RETURN
     C*
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
     C*
      *================================================================
     C     WRTDO         BEGSR
      *================================================================
     C*
     C*
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                MOVELQQDCID    DODCID
     C*****                MOVELQQCIF     DOFLR
     C*****                MOVELQQCISF    DOSFLR
     C*****                MOVEL*BLANKS   DOOSYS
     C*****                MOVEL*BLANKS   DOLIBN
     C*****                MOVEL*BLANKS   DOOFLD
     C*****                MOVEL*BLANKS   DOOSFL
     C*****                MOVEL*BLANKS   DOODCI
     C*****                MOVE *ZEROS    DOARQS
     C*** ???? DOC TYPE ????** THIS MAY BE UPDATED LATER BY A TSI-QUEUE ENTR
     C*
     C*****                MOVELFPDOCT    DODTYP
      *
     C*** NOT FOR FAX *** ???
     C*****                MOVE *ZEROS    DOBYTD
      *
     C*****                MOVEL'FX'      DOWSID
     C*****                MOVE QQDT8     DOSDT
     C*****                MOVE QQFTIM    DOSTMD
     C*****                MOVELQ1USER    DOSREP
     C*****                MOVEL*BLANKS   DOBCHI
     C*****                MOVE QQ#PGS    DOPGS
     C*****                MOVEL*BLANKS   DOOPTF
     C*
     C*****                MOVEL'N'       DOIDFX
     C*****                MOVEL*BLANKS   DOAIFL
     C*
     C*****                MOVEL*BLANKS   DOVOL1
     C*****                MOVEL*BLANKS   DOVOL2
     C*****                MOVEL*BLANKS   DOVOL3
     C*****                MOVEL*BLANKS   DODKEY
     C*****                MOVE *ZEROS    DODSQ#
     C*****                MOVEL*BLANKS   DOLPGM
     C*****                MOVEL*BLANKS   DOLUSR
     C*****                MOVE *ZEROS    DOLDT
     C*****                MOVE *ZEROS    DOLTIM
     C*
     C*****                MOVELQ1USER    DOIUSR
     C*****                MOVELQ1PGM     DOIPGM
     C*****                TIME           DSDTTM
     C*****      DSDATE    MULT 10000.01  DOIDT
     C*****                Z-ADDDSTIME    DOITIM
     C*
     C*****                MOVEL'RL'      DODSTS
     C*****                MOVEL'F'       DOFL01           FAX
     C*
     C*****                WRITEPDO01
     C                   MOVEL     *BLANKS       QQOSYS
     C                   MOVEL     *BLANKS       QQLBN
     C                   MOVEL     *BLANKS       QQOFLD
     C                   MOVEL     *BLANKS       QQOSFL
     C                   MOVEL     *BLANKS       QQODCI
     C                   MOVE      *ZEROS        QQARQS
     C                   MOVEL     FPDOCT        QQDTYP
     C                   MOVE      *ZEROS        QQBYTD
     C                   MOVEL     'FX'          QQWSID
     C                   MOVE      QQDT8         QQSDT
     C                   MOVE      QQFTIM        QQSTMD
     C                   MOVEL     Q1USER        QQSREP
     C                   MOVEL     *BLANKS       QQBCHI
     C                   MOVE      QQ#PGS        QQPGS
     C                   MOVEL     *BLANKS       QQOPTF
     C                   MOVEL     'N'           QQIDFX
     C                   MOVEL     *BLANKS       QQAIFL
     C                   MOVEL     *BLANKS       QQVOL1
     C                   MOVEL     *BLANKS       QQVOL2
     C                   MOVEL     *BLANKS       QQVOL3
     C                   MOVEL     *BLANKS       QQDKEY
     C                   MOVE      *ZEROS        QQDSQ#
     C                   MOVEL     'RL'          QQDSTS
     C                   MOVEL     'F'           QQFL01                         FAX
     C                   MOVE      ' '           QQINDX
     C*
     C                   CALL      'ISA700R'     ISAKEY
     C***** END  CHG ***  10/17/11  **********************************
     C*
      ***********************************
      **** CREATE THE DOCUMENT NOTES HERE ****************************
      ***********************************
     C                   EXSR      CRTNOT
     C*
     C     BOTDO         ENDSR
     C*
      ********************************************************************
      * * * CREATE DOC NOTES *********************************************
      ********************************************************************
     C     CRTNOT        BEGSR
      *
     C*** FILL ALL DOC NOTE FIELDS
      *
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     QQDT8         XXY               4
     C                   MOVE      QQDT8         XXD               2
     C     2             SUBST     QQDT8:5       XXM               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVEL     QQFTIM        XXH               2
     C                   MOVE      QQFTIM        XXS               2
     C     2             SUBST     QQFTIM:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXHMS             8
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    DREA:5        XX75
     C*
     C                   MOVEL     XX75          DN(1)
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****      MDOC      CAT  DODCID:1  XX75      P
     C     MDOC          CAT(P)    QQDCID:1      XX75
     C***** END  CHG ***  10/17/11  **********************************
     C                   CAT(P)    DHBR:5        XX75
     C                   CAT(P)    QQ#PGS:1      XX75
     C*
     C                   MOVEL     XX75          DN(2)
     C*
     C                   MOVEL     *BLANKS       XX75
     C                   MOVEL     XX75          DN(3)
     C*
     C*** CALL PGM TO WRITE DOC NOTES
     C*
      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS
     C*
     C                   MOVE      *BLANKS       Q$RTC             1
     C*
     C                   DO        3             G                 3 0
     C*
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****                MOVELDODCID    $#DCID 12
     C                   MOVEL     QQDCID        $#DCID           12
     C***** END  CHG ***  10/17/11  **********************************
     C                   MOVEL     DN(G)         QQCM75
     C*
     C*
     C     G             IFEQ      3
     C                   MOVE      '1'           Q$RTC             1
     C                   ENDIF
     C*
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM      $#DCID        QQDCID           12              DOCID
     C                   PARM      QQDT8         QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C     BPDOC         TAG
     C*
     C                   ENDDO
     C*
     C*
     C                   ENDSR
