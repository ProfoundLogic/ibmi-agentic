      /if defined(CONVERT)
     D psds          esds                  extname(szq1p)
    D*
     D modea           c                   const('-- Add --')
     D modec           c                   const('- Change -')
     D moded           c                   const('-Display-')
     D pdsnam          c                   const('StoneRiver, Inc.')
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
     D*
     D/copy sPrDate
     D/copy sPrMessage
     D/copy sPrRunCmd
     D/copy sPrString
      /Endif
