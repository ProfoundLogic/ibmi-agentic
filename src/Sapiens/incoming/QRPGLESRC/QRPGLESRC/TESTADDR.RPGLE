     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE TESTADDR - TEST/PRINT OUTPUT FROM SRADDR API.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**   CALL THE SRADDR API AND PRINT RESULTS.                     *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    10/21/11            PROJECT: WC2006               *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  EMN                    1    350
     D                                     DIM(7)
     D  ##EMN1               001    050
     D  ##EMN2               051    100
     D  ##EMN3               101    150
     D  ##EMN4               151    200
     D  ##EMN5               201    250
     D  ##EMN6               251    300
     D  ##EMN7               301    350

     d amnds           ds
     d  ##amn1
     d  ##amn2
     d  ##amn3
     d  ##amn4
     d  ##amn5
     d  ##amn6
     d  ##amn7
     d  ##amn8
     d  ##amn9
     d  amn                          60    overlay(amnds) dim(9) inz(*blanks)
     d y               s              2  0
     d type            s              5
     I*

     C     *ENTRY        PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##ODR             1
     C                   PARM                    ##NODR            1
     C                   PARM                    ##AODR            1
     C                   PARM                    ##PGFL            1
     C                   PARM                    ##DBAF            1
     C* Old parms
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM      *BLANKS       ##EMN1           50
     C                   PARM      *BLANKS       ##EMN2           50
     C                   PARM      *BLANKS       ##EMN3           50
     C                   PARM      *BLANKS       ##EMN4           50
     C                   PARM      *BLANKS       ##EMN5           50
     C                   PARM      *BLANKS       ##EMN6           50
     C                   PARM      *BLANKS       ##EMN7           50
     C                   PARM      *ZERO         ##ADRL            3 0
     C                   PARM      *ZERO         ##NAML            3 0
     C                   PARM      *ZERO         ##FEDL            3 0
     C                   PARM      *BLANK        ##PEO1           57
     C                   PARM      *BLANK        ##PEO2           57
     C                   PARM                    ##PGFL            1
     C                   PARM                    ##CONL            3 0
     C*
     C                   EXCEPT    HEAD
     c                   except    oldseper
     C*
     C     1             DO        7             X                 1 0
     C                   MOVE      *BLANKS       XXHEAD            5
     C     X             IFEQ      ##NAML
     C                   MOVEL(P)  'NAME'        XXHEAD
     C                   ENDIF
     C     X             IFEQ      ##FEDL
     C                   MOVEL(P)  'FED'         XXHEAD
     C                   ENDIF
     C     X             IFEQ      ##ADRL
     C                   MOVEL(P)  'ADDR'        XXHEAD
     C                   ENDIF
     C                   EXCEPT    HEAD1
     C                   ENDDO
     C                   EXCEPT    HEAD2
     C* New Parms
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM      *BLANKS       ##EMN1
     C                   PARM      *BLANKS       ##EMN2
     C                   PARM      *BLANKS       ##EMN3
     C                   PARM      *BLANKS       ##EMN4
     C                   PARM      *BLANKS       ##EMN5
     C                   PARM      *BLANKS       ##EMN6
     C                   PARM      *BLANKS       ##EMN7
     C                   PARM      *ZERO         ##ADRL            3 0
     C                   PARM      *ZERO         ##NAML            3 0
     C                   PARM      *ZERO         ##FEDL            3 0
     C                   PARM      *BLANKS       ##PEO1           57
     C                   PARM      *BLANKS       ##PEO2           57
     C                   PARM                    ##PGFL            1
     C                   PARM      *ZERO         ##CONL            3 0
     C                   PARM      *BLANKS       ##AMN1           60
     C                   PARM      *BLANKS       ##AMN2           60
     C                   PARM      *BLANKS       ##AMN3           60
     C                   PARM      *BLANKS       ##AMN4           60
     C                   PARM      *BLANKS       ##AMN5           60
     C                   PARM      *BLANKS       ##AMN6           60
     C                   PARM      *BLANKS       ##AMN7           60
     C                   PARM      *BLANKS       ##AMN8           60
     C                   PARM      *BLANKS       ##AMN9           60
     C                   PARM      *ZERO         ##PEOL            3 0
     C                   PARM                    ##DBAF            1
     C                   PARM      *ZERO         ##AL2             3 0
     C                   PARM      *ZERO         ##NL2             3 0
     C                   PARM      *ZERO         ##FL2             3 0
     C                   PARM      *ZERO         ##PL2             3 0
     C                   PARM      *ZERO         ##CL2             3 0
     C*
     C*
      /free
       y = 1;
       except newSeper;
       dow y < 10;
       select;
       when y = ##adrl;
          type = 'Addr';
       when y = ##naml;
          type = 'Name';
       when y = ##fedl;
          type = 'Fedr';
       when y = ##Peol;
          type = 'PeoN';
       when y = ##Conl;
          type = 'Cont';
       other;
          type = *blanks;
       endsl;
       except newParms;
       y += 1;
       enddo;
      /end-free
     C                   MOVE      '1'           *INLR
     C*
     C*
     OQSYSPRT   E            HEAD           2 06
     O                                           10 'ORDER:'
     O                       ##ODR               12
     O                                           26 'NAME ORDER:'
     O                       ##NODR              28
     O                                           42 'ADDR ORDER:'
     O                       ##AODR              44
     O                                           58 'PEO OUTPUT:'
     O                       ##PGFL              60
     O                                           74 '  DBA FLAG:'
     O                       ##DBAF              76
     o          e            oldSeper    1  1
     o                                           10 'Old Parms'
     O          E            HEAD1          1
     O                       XXHEAD              10
     O                       X                   15
     O                       EMN(X)              70
     O          E            HEAD2       1  1
     O                                           10 'PEO1'
     O                       ##PEO1              77
     O          E            HEAD2          1
     O                                           10 'PEO2'
     O                       ##PEO2              77
     o          e            newSeper    1  1
     o                                           10 'New Parms'
     o          e            newParms       1
     o                       type                10
     O                       y             4     15
     o                       amn(y)              77
