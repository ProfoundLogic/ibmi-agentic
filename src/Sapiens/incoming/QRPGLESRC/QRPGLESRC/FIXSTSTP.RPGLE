     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMAUP - INITIALIZE POLICY TO DATES                     ND   R
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     THIS PROGRAM ASSIGNS THE CORRECT NCCI NUMBER TO STSTP     *
     H*     BASED ON STKEY.                                           *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:87                                            *
     H*   CREATED:    06/11/98    PROJECT:                            *
     H*                                                               *
     H*****************************************************************
     FSTSTL1    UF   E           K DISK

     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     STKY          SETLL     PST01
     C                   READ      PST01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   SELECT
     C*
     C     STKEY         WHENEQ    'AL'
     C                   MOVE      '01'          STNST
     C*
     C     STKEY         WHENEQ    'AZ'
     C                   MOVE      '02'          STNST
     C*
     C     STKEY         WHENEQ    'AR'
     C                   MOVE      '03'          STNST
     C*
     C     STKEY         WHENEQ    'CA'
     C                   MOVE      '04'          STNST
     C*
     C     STKEY         WHENEQ    'CO'
     C                   MOVE      '05'          STNST
     C*
     C     STKEY         WHENEQ    'CT'
     C                   MOVE      '06'          STNST
     C*
     C     STKEY         WHENEQ    'DE'
     C                   MOVE      '07'          STNST
     C*
     C     STKEY         WHENEQ    'DC'
     C                   MOVE      '08'          STNST
     C*
     C     STKEY         WHENEQ    'FL'
     C                   MOVE      '09'          STNST
     C*
     C     STKEY         WHENEQ    'GA'
     C                   MOVE      '10'          STNST
     C*
     C     STKEY         WHENEQ    'ID'
     C                   MOVE      '11'          STNST
     C*
     C     STKEY         WHENEQ    'IL'
     C                   MOVE      '12'          STNST
     C*
     C     STKEY         WHENEQ    'IN'
     C                   MOVE      '13'          STNST
     C*
     C     STKEY         WHENEQ    'IA'
     C                   MOVE      '14'          STNST
     C*
     C     STKEY         WHENEQ    'KS'
     C                   MOVE      '15'          STNST
     C*
     C     STKEY         WHENEQ    'KY'
     C                   MOVE      '16'          STNST
     C*
     C     STKEY         WHENEQ    'LA'
     C                   MOVE      '17'          STNST
     C*
     C     STKEY         WHENEQ    'ME'
     C                   MOVE      '18'          STNST
     C*
     C     STKEY         WHENEQ    'MD'
     C                   MOVE      '19'          STNST
     C*
     C     STKEY         WHENEQ    'MA'
     C                   MOVE      '20'          STNST
     C*
     C     STKEY         WHENEQ    'MI'
     C                   MOVE      '21'          STNST
     C*
     C     STKEY         WHENEQ    'MN'
     C                   MOVE      '22'          STNST
     C*
     C     STKEY         WHENEQ    'MS'
     C                   MOVE      '23'          STNST
     C*
     C     STKEY         WHENEQ    'MO'
     C                   MOVE      '24'          STNST
     C*
     C     STKEY         WHENEQ    'MT'
     C                   MOVE      '25'          STNST
     C*
     C     STKEY         WHENEQ    'NE'
     C                   MOVE      '26'          STNST
     C*
     C     STKEY         WHENEQ    'NV'
     C                   MOVE      '27'          STNST
     C*
     C     STKEY         WHENEQ    'NH'
     C                   MOVE      '28'          STNST
     C*
     C     STKEY         WHENEQ    'NJ'
     C                   MOVE      '29'          STNST
     C*
     C     STKEY         WHENEQ    'NM'
     C                   MOVE      '30'          STNST
     C*
     C     STKEY         WHENEQ    'NY'
     C                   MOVE      '31'          STNST
     C*
     C     STKEY         WHENEQ    'NC'
     C                   MOVE      '32'          STNST
     C*
     C     STKEY         WHENEQ    'ND'
     C                   MOVE      '33'          STNST
     C*
     C     STKEY         WHENEQ    'OH'
     C                   MOVE      '34'          STNST
     C*
     C     STKEY         WHENEQ    'OK'
     C                   MOVE      '35'          STNST
     C*
     C     STKEY         WHENEQ    'OR'
     C                   MOVE      '36'          STNST
     C*
     C     STKEY         WHENEQ    'PA'
     C                   MOVE      '37'          STNST
     C*
     C     STKEY         WHENEQ    'RI'
     C                   MOVE      '38'          STNST
     C*
     C     STKEY         WHENEQ    'SC'
     C                   MOVE      '39'          STNST
     C*
     C     STKEY         WHENEQ    'SD'
     C                   MOVE      '40'          STNST
     C*
     C     STKEY         WHENEQ    'TN'
     C                   MOVE      '41'          STNST
     C*
     C     STKEY         WHENEQ    'TX'
     C                   MOVE      '42'          STNST
     C*
     C     STKEY         WHENEQ    'UT'
     C                   MOVE      '43'          STNST
     C*
     C     STKEY         WHENEQ    'VT'
     C                   MOVE      '44'          STNST
     C*
     C     STKEY         WHENEQ    'VA'
     C                   MOVE      '45'          STNST
     C*
     C     STKEY         WHENEQ    'WA'
     C                   MOVE      '46'          STNST
     C*
     C     STKEY         WHENEQ    'WV'
     C                   MOVE      '47'          STNST
     C*
     C     STKEY         WHENEQ    'WI'
     C                   MOVE      '48'          STNST
     C*
     C     STKEY         WHENEQ    'WY'
     C                   MOVE      '49'          STNST
     C*
     C     STKEY         WHENEQ    'PR'
     C                   MOVE      '50'          STNST
     C*
     C     STKEY         WHENEQ    'ZZ'
     C                   MOVE      '51'          STNST
     C*
     C     STKEY         WHENEQ    'HI'
     C                   MOVE      '52'          STNST
     C*
     C     STKEY         WHENEQ    'ZZ'
     C                   MOVE      '53'          STNST
     C*
     C     STKEY         WHENEQ    'AK'
     C                   MOVE      '54'          STNST
     C*
     C                   ENDSL
     C*
     C                   UPDATE    PST01
     C*
     C                   READ      PST01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C****************************************************************
     C* *INZSR - INITIALIZATION SUBROUTINE
     C****************************************************************
     C     *INZSR        BEGSR
     C*
     C                   MOVE      *LOVAL        KKKEY            10
     C*
     C                   ENDSR
