     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H**** USE CPY STREAM FILE THIS TIME
     F*
      * CREATE A DIRECTORY ON OPTICAL ***

     D**
     D NPATH           DS
     D  NPFSYS                 1      5
     D  NVOLPT                 6     80
     D NPTHLN          DS
     D  NPTHL                  1      4B 0
     D*
     D ATRTBL          DS
     D  ATR#A                  1      4B 0
     D ATRLEN          DS
     D  ATRL                   1      4B 0
     D*
     D*
     D*
      *-------------------------------------------*
      *    RETCD HAS THE FOLLOWING INFO           *
      *-------------------------------------------*
      *** INPUT    BINARY(4)     BYTES PROVIDED   -   0 =EXCEPTION HANDLING
      *                                            else =Error Info rtnd
      *
      *** OUTPUT   BINARY(4)     BYTES AVALIBLE   -   0 =No error was rtrnd
      *                                            else =Length of data rtnd
      *
      *** OUTPUT   CHAR(7)       EXCEPTION ID           =Message ID
      *
      *** OUTPUT   CHAR(1)       RESERVED               =nothing
      *
      *** OUTPUT   CHAR(*)       EXCEPTION DATA         =error data rtnd
      *
      *-------------------------------------------*
      *
     D RETCD           DS
     D  RCLEN                  1      4B 0
     D  RTCD                   5      8B 0
     D  CONDTN                 9     15
     D  RSV                   16     16
     D  MSG                   17    272

     I*

     C     PCRTSF        PLIST
     C                   PARM                    NPATH            80
     C                   PARM                    NPTHL
     C                   PARM                    ATRTBL
     C                   PARM                    ATRLEN
     C                   PARM                    RETCD
     C*
     C*
     C     PRMSG         PLIST
     C                   PARM                    QMID              7
     C                   PARM                    QMDTA           256
     C                   PARM                    QMDESC          256
     C                   PARM                    QMRTN             1
     C*
     C*
     C     PLBRK         PLIST
     C                   PARM                    QMID              7
     C                   PARM                    QMDTA           256
     C                   PARM                    QMRTN             1
     C*
     C*
     C     *ENTRY        PLIST                                                  "R"READ OR "W"-
     C                   PARM                    NPTH             63            FRM FLR/DOC   )
     C                   PARM                    MSGDSC          256            ERR MSG DSC
     C                   PARM                    QQRTCD            1            RTCD          )
     C*
     C**** ADD 10/20/02 ********** VIRT OPTICAL PROCESSING *********
     C***** ADD 08/15/02 ***
     C                   EXSR      DBGMOD
     C***** END ADD 08/15/02 ***
     C**** END ADD 10/20/02 ****************************************
     C                   Z-ADD     0             ATR#A
     C                   Z-ADD     4             ATRL
     C*
     C*
     C*
     C                   MOVE      *ZERO         RCLEN
     C                   MOVE      *ZERO         RTCD
     C*
     C                   EXSR      CRTD
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C     CRTD          BEGSR
     C                   MOVEL     '/QOPT'       NPFSYS
     C                   MOVEL     NPTH          NVOLPT
     C                   Z-ADD     80            NPTHL
     C                   Z-ADD     272           RCLEN
     C                   CALL      'QHFCRTDR'    PCRTSF
      *
      *
     C*** CHECK ERROR STUFF ***
     C*
     C     RTCD          IFNE      0
     C                   MOVE      '1'           ERR               1
     C*** PROCESS ERRORS...RETURN THEM TO THE JOB LOG. ETC.
     C*
      * * * DEFAULT ERROR TO RETURN
     C                   MOVE      '3'           QQRTCD
     C*
      * * * VOLUME FULL(THRESHOLD MET)
     C     CONDTN        IFEQ      'CPF1F61'
     C                   MOVE      '1'           QQRTCD
     C                   END
     C*
      * * * PATH NAME INVALID
     C     CONDTN        IFEQ      'CPF1F48'
     C                   MOVE      '2'           QQRTCD
     C                   END
     C*
     C*** CALL RTV MSG PGM TO SND PGM MSG OF ERROR AND RETRIEVE THE
     C*** DESCRIPTION.
     C                   MOVEL     CONDTN        QMID
     C                   MOVEL     MSG           QMDTA
     C                   MOVEL     *BLANKS       QMDESC
     C                   MOVEL     *BLANKS       QMRTN
     C*
     C                   CALL      'RTVMSGC'     PRMSG
     C     QMRTN         IFEQ      *BLANKS
     C                   MOVE      QMDESC        MSGDSC
     C                   ELSE
     C                   MOVE      *BLANKS       MSGDSC
     C                   END
     C*
     C     CONDTN        IFEQ      'CPF1F71'                                          F SYS
     C                   CALL      'BRKMSGC'     PLBRK
     C*
     C     QMRTN         IFEQ      '*'
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C                   END
     C     CPYEND        ENDSR
A    C*
     C*** ADD 08/15/02 ******
      *====================================================================*
      **** DEBUG MODE (OR VIRTUAL OPTICAL PROCESS)
     C     DBGMOD        BEGSR
     C     *DTAARA       DEFINE                  VRTOPT            1
     C                   IN        *DTAARA                              83      ERROR IF 83
     C     *IN83         IFEQ      *OFF
     C*** FOUND THE DTAARA, SO CHECK FLAG TO SEE IF WE SHOULD USE VIRTUAL
     C**  OPTICAL PROCESSING
     C     VRTOPT        IFEQ      'Y'
     C** DO VRTOPT PROCESS
     C                   CALL      'VRTOPTD'                            99
     C                   PARM                    NPTH             63            FRM FLR/DOC   )
     C                   PARM                    MSGDSC          256            ERR MSG DSC
     C                   PARM                    QQRTCD            1            RTCD          )
     C     *IN99         CABEQ     *ON           ENDDBG
     C*
     C                   GOTO      TERM
     C                   ELSE
     C                   GOTO      ENDDBG
     C                   ENDIF
     C                   ELSE
     C*** NO DATA AREA, SO NO VIRT OPTICAL PROCESSING DO NORMAL
     C                   GOTO      ENDDBG
     C                   ENDIF
     C*
     C     ENDDBG        ENDSR
     C*
     C*** END ADD 08/15/02 ******
