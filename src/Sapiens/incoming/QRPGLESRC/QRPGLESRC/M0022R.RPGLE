    Hcopyright('(C) Copyright Fiserv, Inc., 2006')
    H*********************************************************************
    H/TITLE Set Environment
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 05/01/06      C:33      PROJECT: SY082                  *
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fmmllp     if   e             disk    rename(pll01:pll0p)
    F*
    D*
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D command         s            255
     D exitKC          s              1n
     D exitKL          s              1n
     D srchRRN         s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*entry
     D M0022R          pr                  extPgm('M0022R')
     D M0022R          pi
     D*
     D/copy sPrRunCmd
     D*
     D SrchEnv         pr                  extPgm('M0021R')
     D  call                          1    const
     D  kc                            1n
     D  kl                            1n
     D  rrn                           9  0
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C                   dou       exitKC or exitKL or srchRRN <> *zero
     C*
     C                   eval      srchRRN = *zero
     C*
     C                   callp     SrchEnv('2':exitKC:exitKL:srchRRN)
     C*
     C                   enddo
     C*
     C                   if        srchRRN <> *zero
     C*
     C                   clear                   LLDTAA
     C*
     C     srchRRN       chain     MMLLP
     C*
     C                   eval      command = 'CRTDTAARA DTAARA(QTEMP/' +
     C                                       q1User + ') TYPE(*CHAR)'
     C                   callp(e)  RunCmd(command:%len(command))
     C*
     C                   eval      command = 'CHGDTAARA DTAARA(QTEMP/' +
     C                                       q1User + ') VALUE(''' +
     C                                       LLDTAA + LLAPP + ''')'
     C                   callp     RunCmd(command:%len(command))
     C*
     C                   endif
     C*
     C                   eval      *inlr = *on
