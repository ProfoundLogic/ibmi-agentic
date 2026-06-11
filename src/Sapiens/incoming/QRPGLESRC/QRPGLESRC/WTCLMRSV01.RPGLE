     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Claim reserve history detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMRSV
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:191 for Project Transform
     h**********************************************************************
     h* Revisions:
     h* 07/20/20 C: 079 Project: WC2749
     h*                 E-Pay - Recompile only
     h*
     h*
     h**********************************************************************
      * Customer Master File
     Fwmrhp     if   e             disk    rename(prh01:prh0p)
     FWmrhl     if   e           k Disk    ReName(prh01:prh00)
     fwdpal     if   e           k Disk    ReName(ppa01:ppa00)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY
     DClaimReserve01   pr                  ExtPgm('WTCLMRSV01')
     D                                9  0
     D                            32767
     D                                1
      *
     DClaimReserve01   pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     D numEdtCde       c                   const('N')
      *
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  RHrt01                 1      2
     D  RHrt02                 3      4
     D  RHrt03                 5      6
     D  RHrt04                 7      8
     D  RHrt05                 9     10
     D  RHrt06                11     12
     D  RHrt07                13     14
     D  RHrt08                15     16
     D  RHrt09                17     18
     D  RHrt10                19     20

     D                 DS
     D  SU                     1     90  2
     D                                     DIM(10)
     D  RHsu01                 1      9  2
     D  RHsu02                10     18  2
     D  RHsu03                19     27  2
     D  RHsu04                28     36  2
     D  RHsu05                37     45  2
     D  RHsu06                46     54  2
     D  RHsu07                55     63  2
     D  RHsu08                64     72  2
     D  RHsu09                73     81  2
     D  RHsu10                82     90  2
      *
     D strKey          s             10
     D z               s              2  0
     D intCo#          s                   like(rhco#)
     D intFnd          s                   like(rhfnd)
     D intFyr          s                   like(rhfyr)
     D intCase         s                   like(rhCase)
     D strType         s              1
      *
     D intEfDt         s              7  0
     D intTime         s                   like(RHtime)
     D decCcHg         s              9  2
     D decMcHg         s              9  2
     D decLcHg         s              9  2
     D decEcHg         s              9  2
     D decCrSv         s              9  2
     D decMrSv         s              9  2
     D decLrSv         s              9  2
     D decErSv         s              9  2
     D decCiNc         s              9  2
     D decMiNc         s              9  2
     D decLiNc         s              9  2
     D decEiNc         s              9  2
     D decToTi         s             10  2
     D decAmt          s                   like(PAAmt)
      *
      /Free

       Chain(n) inRRN prh0p;
        // set key values
       intCo# = RHCo#;
       intFnd = RHFnd;
       intFyr = RHFyr;
       intCase = RHCase;

       outJSON = ' ';
       BuildJSON(outJSON :'OutDtlDatCh' :%EditW(dtMDYY(RHefdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlUser' :%TrimR(GetUsrName(RHuser)));
       BuildJSON(outJSON :'LnkUsrEmal' :%TrimR(GetUsrEmail(RHuser)));
       BuildJSON(outJSON :'OutDtlRsn'   :%TrimR(RHexpl));

        //calculate detail fields
       decCcHg = 0;
       decMcHg = 0;
       decLcHg = 0;
       decEcHg = 0;

       Exsr BuildData;

        //Build JSON Output
       BuildJSON(outJSON :'OutDtlComp' :%EditC(decCcHg:numEdtcde));
       BuildJSON(outJSON :'OutDtlMedic' :%EditC(decMcHg:numEdtcde));
       BuildJSON(outJSON :'OutDtlLegal' :%EditC(decLcHg:numEdtcde));
       BuildJSON(outJSON :'OutDtlExpen' :%EditC(decEcHg:numEdtcde));

       BuildJSON(outJSON :'OutDtloutc' :%EditC(decCrSv:numEdtcde));
       BuildJSON(outJSON :'OutDtlOutM' :%EditC(decMrSv:numEdtcde));
       BuildJSON(outJSON :'OutDtlOutL' :%EditC(decLrSv:numEdtcde));
       BuildJSON(outJSON :'OutDtlOutE' :%EditC(decErSv:numEdtcde));

       BuildJSON(outJSON :'OutDtlTtlC' :%EditC(decCiNc:numEdtcde));
       BuildJSON(outJSON :'OutDtlTtlM' :%EditC(decMiNc:numEdtcde));
       BuildJSON(outJSON :'OutDtlTtlL' :%EditC(decLiNc:numEdtcde));
       BuildJSON(outJSON :'OutDtlTtlE' :%EditC(decEiNc:numEdtcde));
       BuildJSON(outJSON :'OutDtlTotal' :%EditC(decToTi:numEdtcde));

       RtnCode = ' ';
       Return;
      /End-Free

      *************************************************************
      * BuildData for detail box
      *************************************************************
     C     BuildData     BegSr
     C*
     C     ClmKey        klist
     C                   kfld                    intCo#
     C                   kfld                    intFnd
     C                   kfld                    intFyr
     C                   kfld                    intCase
      *
     C                   For       z = 1 to 10
     C                   If        RT(z) <> *blanks
     C                   Eval      strKey = RT(Z)
     C                   Eval      strType = GetRsvType(RHCo#:strKey)
     C*
     C                   Select
     C                   When      strType = 'C'
     C                   ADD       SU(Z)         decCcHg
     C                   When      strType = 'M'
     C                   ADD       SU(Z)         decMcHg
     C                   When      strType = 'L'
     C                   ADD       SU(Z)         decLcHg
     C                   When      strType = 'E'
     C                   ADD       SU(Z)         decEcHg
     C                   EndSl
     C*
     C                   EndIf
     C                   EndFor
     C*
     C                   Z-ADD     0             decCrSv
     C                   Z-ADD     0             decMrSv
     C                   Z-ADD     0             decLrSv
     C                   Z-ADD     0             decErSv
     C                   Z-ADD     0             decCiNc
     C                   Z-ADD     0             decMiNc
     C                   Z-ADD     0             decLiNc
     C                   Z-ADD     0             decEiNc
     C                   Z-ADD     0             decToTi
     C                   Z-ADD     RHEFDT        intEfDt
     C                   Z-ADD     RHTIME        intTime
     C
     C
     C*  GET THE SUM OF ALL RESERVE CHANGES UP TO THE DATE AND TIME OF
     C*  THE RECORD BEING DISPLAYED.
     C
     C     ClmKey        SETLL     PRH00
     C     ClmKey        READE     PRH00                                  50
     C     *IN50         DOWEQ     '0'
     C     RHEFDT        COMP      intEfDt                            41  40
     C   40RHTIME        COMP      intTime                            41
     C     *IN41         IFEQ      '0'
     C
     C                   For       z = 1 to 10
     C
     C                   If        RT(z) <> *blanks
     C                   Eval      strKey = RT(Z)
     C                   Eval      strType = GetRsvType(RHCo#:strKey)
     C
     C                   Select
     C                   When      strType = 'C'
     C                   ADD       SU(Z)         decCrSv
     C                   ADD       SU(Z)         decCiNc
     C                   When      strType = 'M'
     C                   ADD       SU(Z)         decMrSv
     C                   ADD       SU(Z)         decMiNc
     C                   When      strType = 'L'
     C                   ADD       SU(Z)         decLrSv
     C                   ADD       SU(Z)         decLiNc
     C                   When      strType = 'E'
     C                   ADD       SU(Z)         decErSv
     C                   ADD       SU(Z)         decEiNc
     C                   EndSl
      *
     C                   ADD       SU(Z)         decToTi
     C                   EndIf
     C                   EndFor
     C                   EndIf
     C
     C     ClmKey        READE     PRH00                                  50
     C                   EndDo
     C
     C*  SUBTRACT ALL PAYMENTS UP TO THE DATE AND TIME OF THE RECORD
     C*  BEING DISPLAYED.
     C
     C     ClmKey        CHAIN     PPA00                              50
     C     *IN50         DOWEQ     '0'
     C     PATRDT        COMP      intEfDt                            41  41
     C     *IN41         IFEQ      '0'
     C
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C                   Z-ADD     PAAMT         decAmt
     C                   ELSE
     C                   Z-SUB     PAAMT         decAmt
     C                   ENDIF
     C
     C                   Select
     C                   When      PARetp = 'C'
     C                   SUB       decAmt        decCrSv
     C                   When      PARetp = 'M'
     C                   SUB       decAmt        decMrSv
     C                   When      PARetp = 'L'
     C                   SUB       decAmt        decLrSv
     C                   When      PARetp = 'E'
     C                   SUB       decAmt        decErSv
     C                   EndSl
     C                   EndIf
     C
     C     ClmKey        READE     PPA00                                  50
     C                   EndDo
     C
     C                   EndSr
