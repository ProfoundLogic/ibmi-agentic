     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SCCV8R1- CONVERT DOCS TO .PDF FORMAT FOR DARTS (OPTICAL ONLY)
      *
      **** PROCESS FOR OL AND MD FILES
      * * *
      ***************************************************************
      * CALLS TO/PARMS
      **** SCCV2R (COPIED FROM IS828R)
      *
      ***************************************************************
      *  REVISIONS:
     *                                                                *
     * 04/15/14  C:300 Project: SY195                                 *
     *                 Update Error messages to provide correct info  *
     *                                                                *
     * 11/02/10  C:183 Project: SY195                                 *
     *                 Remove hard coding for QDLS. Call RTVDOCROOT to*
     *                 get the root folder name.                      *
     *                                                                *
     H***************************************************************
     F*
     FSXC2L1    UF   E           K DISK

     D*
     D ERM             S             28    DIM(6) CTDATA PERRCD(1)
     D MSG             S             80    DIM(5) CTDATA PERRCD(1)
     D NR              S             40    DIM(2) CTDATA PERRCD(1)
      *
     D @@FPTH          DS
     D  @FPRE                  1      5
     D  SVOLPT                 6     80
     D SPTHLN          DS
     D  SPTHL                  1      4B 0
     D*
     D @@TPTH          DS
     D  @TPRE                  1      5
     D  TVOLPT                 6     80
     D @@TPT2          DS
     D  @TPR2                  1      9
     D  TVOLP2                10     80
     D TPTHLN          DS
     D  TPTHL                  1      4B 0
     D*
     D CPYINF          DS
     D  TYPOFC                 1      1
     D  CPYFIL                 2      6
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      * * *  END PROGRAM CONTROL DATA AREA (NON BLANK MEANS END PROGRAM)
      * * *    X#FLG = THIS PROGRAM CONTROL  X#FLG2 = DELETE PROCESS CTL
     D SCCV           UDS
     D  X#FLG                  1      1
     D  X#FLG2                 2      2
     D  X#FLG3                 3      3
     D  X#BLNK                 4    256
     D*
     D                 DS
     D  ERDS                   1     40
     D  ERPGM                  1      8
     D  ERBLK1                 9      9
     D  ERRTCD                10     10
     D  ERBLK2                11     11
     D  ERDESC                12     40
      *** Beg add ***** 11/02/10 ***********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
      *** End add ***** 11/02/10 ***********************************************
      * =================================== *
      * * *  KEY LISTS                      *
      * =================================== *

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
     C     CVKY          KLIST
     C                   KFLD                    QQVOL
     C                   KFLD                    XXKEY            40
      *
     C*
     C     SVK           KLIST
     C                   KFLD                    QQVOL
     C                   KFLD                    OLKEY
     C                   KFLD                    OLSEQ#
     C                   KFLD                    OLOFLR
     C                   KFLD                    OLOID
      *
     C*
     C     C1K1          KLIST
     C                   KFLD                    QQVOL
      *
     C*
     C*** DARTS RECORD
      *
     C     CMKY          KLIST
     C                   KFLD                    XXKEY            40
     C                   KFLD                    XXSEQ#            5 0
      *
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C     DLKY          KLIST
     C                   KFLD                    XXKEY            40
     C                   KFLD                    XXSEQ#            5 0
     C*
      *
      *
     C*
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *
      *
      *** PARM LIST FOR CALL TO SCCV9R PROGRAM
      *
     C     PLCV8         PLIST
     C                   PARM                    @@FPTH           80            FROM  PATH
     C                   PARM                    @@TPTH           80            TO    PATH
     C                   PARM      *BLANKS       RTCD              1            RETURN VALUES
      * @@FPTH (STORE PATH) FORMAT IS:  /QOPT/OOVL/OFLR/OSUB/OBJN
      * @@FPTH (STORE PATH) FORMAT IS:  /QDLS/FOLDER/DCID
      *
      *** PARM LIST FOR CALL TO SCCV9R PROGRAM
      *
     C     PLCV8A        PLIST
     C                   PARM                    @@FPTH           80            FROM  PATH
     C                   PARM                    @@TPT2           80            TO    PATH
     C                   PARM      *BLANKS       RTCD              1            RETURN VALUES
      * @@FPTH (STORE PATH) FORMAT IS:  /QOPT/OOVL/OFLR/OSUB/OBJN
      * @@FPTH (STORE PATH) FORMAT IS:  /OPTMLB01/OOVL/OFLR/OSUB/OBJN
      *
      *** PARM LIST FOR CALL TO SVALIFS PGM
      *
     C     PLVI          PLIST
     C                   PARM                    VOBJ             80            FROM  PATH
     C                   PARM      *BLANKS       VRTN              1            RTN CODE
      *** PARM LIST FOR CALL TO SMDIFS PROGRAM
      *
     C     PLMD          PLIST
     C                   PARM                    VOBJ             80            FROM  PATH
     C                   PARM      *BLANKS       MRTN              1            RTN CODE
      *
      *
      *** PARM LIST FOR CALL TO MESSAGE PGM
      *
     C     PLCV3         PLIST
     C                   PARM                    QQMSG1           80            MSG TO SND
     C*
     C*** ENTRY PARM LIST
     C     *ENTRY        PLIST
     C                   PARM                    QQWSID            2
     C                   PARM                    QQPRE            13
     C                   PARM                    QQVOL            10
     C                   PARM                    QQCNT             5
      *
      *** Beg chg ***** 11/02/10 ***********************************************
      * Call the Root determination program
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
     C*******    '\QDLS\'  CAT  QQPRE     DLSPRE 19 P
     C     '\'           CAT(P)    ROOT          RPATH            12            Root path
     C     RPATH         CAT(P)    '\':0         RPATH
     C     RPATH         CAT(P)    QQPRE:0       DLSPRE           19
      *** End chg ***** 11/02/10 ***********************************************
      *
     C                   MOVE      QQCNT         COUNT             5 0
     C                   UNLOCK    SCCV
      *
     C     *DTAARA       DEFINE                  SCCV
      *
      *** READ EACH RECORD SELECTED FOR PURGE.
     C*
     C     C1K1          CHAIN     PC201                              60
     C*******************  READ PCV01                    60
     C*
     C     *IN60         DOWEQ     '0'
     C     COUNT         IFGE      9900
     C                   MOVEL     MSG(1)        QQMSG1
     C*** CAN'T CREATE MORE SPLFS THAN THIS, SO RESTART
     C                   CALL      'SCCV3C'      PLCV3
     C*
     C                   MOVEL     'R '          QQWSID
     C                   GOTO      TERM
     C***
     C                   ENDIF
     C*
     *** CHECK FOR END OF JOB REQUEST (USE DATA AREA FLAG?)
     ***       CLEAR FLAG, END PGM
     C                   IN        SCCV
     C     X#FLG         IFNE      *BLANK
     C     *LOCK         IN        SCCV
     C                   MOVE      *BLANK        X#FLG
     C                   OUT       SCCV
     C                   GOTO      TERM
     C                   ENDIF
     ***
     C**** CHECK HERE TO SEE IF THIS NEEDS TO BE CONVERTED******************
     C****   IF NOT, THEN COPY FROM OPTICAL TO IT'S ORIGINAL LOCATION (CM) AND
     C****   DOCID (DCID OR IDOC)  - UPDATE THE NEW FIELDS
     C******
     C******  IF SO, THEN RUN CONVERSION ON IT (LIKE BEFORE), AND UPDATE THE NEW
     C***     FIELDS
     C**** ADDED THIS --->>> IF CVERR = NOCM  (NO CM RECORD FOUND, JUST COPY OBJ
     C****   OBJECT TO THE IFS/OPTMLB01... IN ORIGINAL LOCATION
     C****     DONT UPDATE CMFL05 FOR THIS ONE...
     C     CVERR         IFEQ      NR(2)
     C*
     C                   EXSR      NOCM
     C                   GOTO      NXTONE
     C*
     C                   ENDIF
     C*
     C*** NEVER CONVERT FOR THIS TYPE OF RECORDS (OL/MD)
     C                   EXSR      NOCVT
     C*
     C***************** SEPERATE PROCESS WILL ACTUALLY UPDATE SDCMP WITH THE
     C****************   APPROPRIATE INFO.
     C****
     *** -------> PROCESS IS DONE HERE... UPDATE FILES APPROPRIATELY
     **            C1FILE - NEW DCID/FOLDER NO OPTICAL INFO
     C*
     C** UPDATE THE SXCVP FILE HERE INC. INFO FOR OPTICAL IF NECCESARY
     C***
     C**** FIELDS TO UPDATE, OPTICAL FLAG (POSSIBLE), NEWDOC/NEWFLR, STATS
     C**** LMFIELDS???
     C                   MOVEL     XXDCID        CVNDOC
     C                   MOVEL     XXNPTH        CVNFLD
     C*** NO FLAG IN MD THAT SPECIFIES OPTICAL (THIS COMES FROM CM RC)
     C********             MOVEL*BLANK    MDFL05
     C                   MOVEL     'C'           CVPRCS
     C*
     C                   MOVE      *BLANKS       CVERR
     C                   MOVE      *BLANKS       CVEFLG
     C*
     C                   UPDATE    PC201
      *
     C     NXTONE        TAG
     C     C1K1          READE     PC201                                  60
     C**************       READ PCV01                    60
     C                   ENDDO
     C*
     **** HERE WE CAN PROCESS THE C2 FILE ***8
     **** SAME AS ABOVE, MOST SHOULD NOT BE OV DOCS, SO THEY WILL BE
     ***8   RESTORED TO THEIR ORIGINAL LOCATION.
     ***
     ***
     ***
     C     TERM          TAG
     C                   MOVE      COUNT         QQCNT
     C                   MOVE      *ON           *INLR
     **===================================================================
     **===================================================================
     **===================================================================
     C****   IF NOT, THEN COPY FROM OPTICAL TO IT'S ORIGINAL LOCATION (CM) AND
     C****   DOCID (DCID OR IDOC)  - UPDATE THE NEW FIELDS
     C**** SUB TO NOT CONVERT
     C     NOCVT         BEGSR
     C*
     C** MOVE CM VALS INTO THE "TO" INFORMATION
     C** OPTICAL VALUES INTO THE "FROM " INFORMATION
     C*** CALL PROGARM TO COPY, GET ANY ERRORS AND MAKE SURE COPY OCCURS, ELSE
     C*** FLAG AS ERROR.
     C*
     C                   MOVEL     '/QOPT'       @FPRE
      *** Beg chg ***** 11/02/10 ***********************************************
     C************         MOVEL'/QDLS'   @TPRE
     C     '/'           CAT(P)    ROOT          RPATH                          Root path
     C                   MOVEL     RPATH         @TPRE
      *** End chg ***** 11/02/10 ***********************************************
     C*
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    OLVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLOSBD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLDCID:0      SVOLPT
     C*** TO PATH
     C*
     C     '/'           CAT(P)    OLOFLR:0      TVOLPT
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    OLOID:0       TVOLPT
     C****
     C                   CALL      'SCCV9R'      PLCV8
     C*** SOME ERROR OCCURED
     C     RTCD          IFNE      *BLANKS
     *** ERRORS UPDATE THE SXC1P RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV9R  '    ERPGM
     C                   MOVEL     RTCD          ERRTCD
     C                   MOVEL     ERM(4)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PC201
     C                   GOTO      NXTONE
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       CVEFLG
    CC*
     C                   ENDIF
     C**** FIELDS TO UPDATE IN THE C1 FILE
     C                   MOVEL     OLOID         XXDCID           12
     C                   MOVEL     OLOFLR        XXNPTH           80
     C*
     C     ENOCVT        ENDSR
     **===================================================================
     **===================================================================
     C****   NO CM RECORD COPY FROM OPTICAL TO IT'S DL NAME/LOCATION
     C     NOCM          BEGSR
     C*
     C** MOVE CM VALS INTO THE "TO" INFORMATION
     C** OPTICAL VALUES INTO THE "FROM " INFORMATION
     C*** CALL PROGARM TO COPY, GET ANY ERRORS AND MAKE SURE COPY OCCURS, ELSE
     C*** FLAG AS ERROR.
     C*
     C                   MOVEL     '/QOPT'       @FPRE
      *** Beg chg ***** 11/02/10 ***********************************************
     C************         MOVEL'/QDLS'   @TPRE
     C     '/'           CAT(P)    ROOT          RPATH                          Root path
     C                   MOVEL     RPATH         @TPRE
      *** End chg ***** 11/02/10 ***********************************************
     C                   MOVEL     '/OPTMLB0'    @TPR2
     C                   MOVE      '1'           @TPR2
     C*
     C*
     C                   MOVE      *BLANKS       VOBJ
     C     @TPR2         CAT(P)    '/':0         VOBJ
     C                   CAT(P)    OLVOL1:0      VOBJ             80
      * *** VALIDATE THE DIRECTORIES WILL EXIST BEFORE COPY
     C                   CALL      'SVALIFS'     PLVI
     C                   ADD       1             COUNT
     C     VRTN          IFNE      *BLANKS
     C*** MAKE DIRECTORY HERE
     C                   CALL      'SMDIFS'      PLMD
     C*
     C     MRTN          IFNE      *BLANKS
     C*??? DO WHAT????
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*
     C                   MOVE      *BLANKS       VOBJ
     C     @TPR2         CAT(P)    '/':0         VOBJ
     C                   CAT(P)    OLVOL1:0      VOBJ
     C                   CAT(P)    '/':0         VOBJ
     C                   CAT(P)    OLOFLD:0      VOBJ
      * *** VALIDATE THE DIRECTORIES WILL EXIST BEFORE COPY
     C                   CALL      'SVALIFS'     PLVI
     C                   ADD       1             COUNT
     C     VRTN          IFNE      *BLANKS
     C*** MAKE DIRECTORY HERE
     C                   CALL      'SMDIFS'      PLMD
     C*
     C     MRTN          IFNE      *BLANKS
     C*??? DO WHAT????
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   MOVE      *BLANKS       VOBJ
     C     @TPR2         CAT(P)    '/':0         VOBJ
     C                   CAT(P)    OLVOL1:0      VOBJ
     C                   CAT(P)    '/':0         VOBJ
     C                   CAT(P)    OLOFLD:0      VOBJ
     C                   CAT(P)    '/':0         VOBJ
     C                   CAT(P)    OLOSBD:0      VOBJ
      * *** VALIDATE THE DIRECTORIES WILL EXIST BEFORE COPY
     C                   CALL      'SVALIFS'     PLVI
     C                   ADD       1             COUNT
     C     VRTN          IFNE      *BLANKS
     C*** MAKE DIRECTORY HERE
     C                   CALL      'SMDIFS'      PLMD
     C*
     C     MRTN          IFNE      *BLANKS
     C*??? DO WHAT????
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    OLVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLOSBD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    OLDCID:0      SVOLPT
     C*** TO PATH
     C*
     C***  CREATE THE SOURCE PATH
     C     '/'           CAT(P)    OLVOL1:0      TVOLP2
     C                   CAT(P)    '/':0         TVOLP2
     C                   CAT(P)    OLOFLD:0      TVOLP2
     C                   CAT(P)    '/':0         TVOLP2
     C                   CAT(P)    OLOSBD:0      TVOLP2
     C                   CAT(P)    '/':0         TVOLP2
     C                   CAT(P)    OLDCID:0      TVOLP2
     C****
     C                   CALL      'SCCV9C'      PLCV8A
     C*****************??? CALL 'SCCV9R'  PLCV8A
     C*** SOME ERROR OCCURED
     C     RTCD          IFNE      *BLANKS
     *** ERRORS UPDATE THE SXC1P RCD WITH ERROR CODE, PGMNAM, DESC., ETC.
     C                   CLEAR                   ERDS
     C                   MOVEL     'SCCV9C  '    ERPGM
     C                   MOVEL     RTCD          ERRTCD
     C                   MOVEL     ERM(6)        ERDESC
     C                   MOVEL     ERDS          CVERR
     C                   MOVEL     'E'           CVEFLG
     C                   UPDATE    PC201
     C                   GOTO      NXTONE
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       CVEFLG
    CC*
     C                   ENDIF
     C**** FIELDS TO UPDATE IN THE C1 FILE
     C                   MOVEL     OLDCID        XXDCID
     C                   MOVEL     TVOLP2        XXNPTH
     C*******
     C                   MOVEL     OLDCID        CVNDOC
     C                   MOVEL     TVOLP2        CVNFLD
     C                   MOVEL     OLOFLD        CVOFLD
     C                   MOVEL     OLOSBD        CVOSBD
     C                   MOVEL     OLVOL1        CVVOL1
     C**** DONT DO THIS ** MOVEL*BLANK    CMFL05
     C                   MOVEL     'C'           CVPRCS
     C*
     C****** LEAVE ERR     MOVE *BLANKS   CVERR
     C                   MOVE      *BLANKS       CVEFLG
     C*
     C                   UPDATE    PC201
      *
     C*
     C     ENOCM         ENDSR
     **===================================================================
     C***** beg chg ****** 04/15/14 **********************************
      * Removed QDLS/ from ERM(4) and IFS/ from ERM(6)
     C***** end chg ****** 04/15/14 **********************************
**
ERR GET NEW NAM FOR DCID/FLR
ERR PRINT DOC
ERR CREATE PDF FROM SPLF
ERR ON CPY QOPT TO NCVT                                                                     04/15/14
CVT ERR - TRY NO CONVERT
ERR ON CPY QOPT TO NOCM                                                                     04/15/14
**
REACHED MAX SPLFS FOR PDF CONVERT (SCCV8X)JOB.  JOB WILL RESTART ITSELF
REACHED MAX ON WSID INDEXING. END JOB. JOB WILL RESTART ITSELF.
**
01 - NO CM RCD FOUND
02 - NO MD RCD FOUND
