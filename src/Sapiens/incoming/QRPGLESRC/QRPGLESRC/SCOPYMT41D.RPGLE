      /if defined(CONVERT)
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D modea           c                   const('-- Add --')
     D modec           c                   const('- Change -')
     D moded           c                   const('-Display-')
     D pdsnam          c                   const('Stoneriver, Inc.')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             40
     D ##leng          s              3  0
     D cpyflg          s              1
     D dsptyp          s              1
     D outflg          s              1
     D savedt          s              7  0
     D savetm          s              6  0
     D scuser          s             10
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
      /Endif
