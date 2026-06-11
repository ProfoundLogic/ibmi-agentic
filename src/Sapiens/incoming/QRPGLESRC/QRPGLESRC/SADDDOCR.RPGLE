     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE SADDDOCR - Prompt for DTM Template Document
     H*****************************************************************
     H**   Description:                                              **
     H**     This program prompts for a template document to be used **
     H**     with CRTDTMDOC for the auto-doc-generation process.     **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SADDDOC                                                 **
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/11/03 C:61  SUPPORT DT081 39270                            *
     H*                Rewrite with edits for folder and DocId.       *
     H*                                                               *
     H*****************************************************************    ***
      *
      *
     FSADDDOCF  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
      *
      *
      *

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@DOC            12
     C                   PARM                    @@FOLD           40
      *
      *  FOLDER AND DOCUMENT MUST BE VALID
      *
     C                   MOVE      *BLANKS       SCDOC
     C                   MOVE      *BLANKS       SCFOLD
     C                   SETOFF                                       90
      *
     C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR01
     C                   SETOFF                                       90
      *
     C     SCDOC         IFNE      *BLANKS
     C     SCFOLD        ORNE      *BLANKS
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCFOLD        ##FOLD
     C                   CALL      'SVALFLR'
     C                   PARM                    ##FOLD           80
     C                   PARM      *BLANK        ##RTCD            1
     C                   ENDIF
      *
      *    FOLDER MUST EXIST
      *
     C  N90##RTCD        IFEQ      '1'
     C                   SETON                                        90  71
     C                   ENDIF                                                  ##RTCD IFEQ '1'
      *
      *    USER MUST HAVE AUTHORITY TO FOLDER
      *
     C  N90##RTCD        IFEQ      '2'
     C                   SETON                                        90  72
     C                   ENDIF                                                  ##RTCD IFEQ '2'
      *
      *    DOCUMENT MUST BE VALID
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCFOLD        ##FOLD
     C                   MOVEL(P)  SCDOC         ##DCID
     C                   CALL      'SVALDOC'
     C                   PARM                    ##DCID           12
     C                   PARM                    ##FOLD           80
     C                   PARM                    ##RTCD
     C                   ENDIF
      *
     C  N90##RTCD        IFEQ      '1'
     C                   SETON                                        90  73
     C                   ENDIF
      *
     C                   ENDIF                                                  FLR / DOC
      *
     C                   ENDDO
      *
     C     SCDOC         IFEQ      *BLANKS
     C                   MOVEL(P)  '*DEFAULT'    SCDOC
     C                   ENDIF
      *
     C                   MOVE      SCDOC         @@DOC
     C                   MOVE      SCFOLD        @@FOLD
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *
