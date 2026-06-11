     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

     Fw0066f1   cf   e             WORKSTN SFILE(SCR01S:W@Rrn1)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fwdf3p     if a e             disk
    F*
     Fwdf3l     if   e           k disk
     F                                     rename(pf301:dbrcdf)
    F* CO/FND/ST <F3>
     FWXGSL     if   e           k disk
     F*
     DW@Rrn1           S              4  0 Inz
     Denter            S              1  0 Inz
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* ARRAYS
    D*---------------------------------------------------------------------
     Dgp               s              3p 0 dim(70)
     Dnm               s             40    dim(70)
     D@indx            s              9P 0 inz
     D@indx1           s              9P 0 inz
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##rrn           s              9  0
     D kkco#           s                   like(f3co#)
     D kkfnd           s                   like(f3fnd)
     D kkkst           s                   like(f3kst)
     D ##kst           s                   like(f3kst)
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkkst
    C*================================================================
     C*
     C     *dtaara       define    W0066R1       RRN               9 0
     C     *lock         in        RRN
     C*
     C     rrn           chain     pf301                              90
     C                   movel     f3co#         kkco#
     C                   movel     f3kst         kkkst
     C*
     C     *loval        setll     PGS01
     C                   read      PGS01                                  50
     C                   dow       *in50 = *off
     C                   if        gsiefl = 'I'
     C                   move      gsfnd         kkfnd
     C     key1          chain     dbrcdf                             90
     C                   if        *in90 = *OFF
     C                   eval      @indx = @indx + 1
     C                   eval      gp(@indx)  = gsfnd
     C                   eval      nm(@indx)  = gsname
     C                   endif
     C                   eval      *in90 = '0'
     C                   endif
     C                   read      PGS01                                  50
     C                   enddo
     C*
     C*  Subfile processing
     C*
     C                   if        @indx >= 1
     C                   Eval      @indx1  = @indx
     C*
     C                   DoW       *INKl = *Off
     C                   ExSr      #LodSfl
     C                   write     footer
     C                   EVAL      scuser = Q1USER
     C                   if        W@Rrn1 >= 13
     C                   eval      *in97 = *on
     C                   endif
     C                   ExFmt     SCR01C
     C*  Pageup/down handling
     C                   DoW       *INKl = *Off
     C                   if        *in01 = *on or *in02 = *on                   Pageup/down
     C                   eval      *in91 = *on
     C                   ExFmt     SCR01C
     C                   endif                                                  Pageup/down
     C                   if        *in01 = *off and *in02 = *off                Pageup/down
     C                   leave
     C                   endif
     C                   enddo
     C                   leave
     C                   enddo
     C                   endif
     C*
     C*
     C*update/write the records
     C*
     C                   if        *inkl = *off
     C     rrn           chain     pf301                              90
     C                   movel     f3co#         kkco#
     C                   move      f3kst         kkkst
     C*
     C     *loval        setll     PGS01
     C                   read      PGS01                                  50
     C                   dow       *in50 = *off
     C                   if        GSIEFL = 'I'
     C                   move      gsfnd         kkfnd
     C     key1          chain     dbrcdf                             90
     C                   if        *in90 = *on
     C                   move      gsfnd         f3fnd
     C                   write     pf301
     C                   endif
     C                   endif
     C                   read      PGS01                                  50
     C                   enddo
     C                   endif
     C                   Eval      *InLr = *On
     C                   return
     C**********************************************************************************************
     C*  Load subfile
     C**********************************************************************************************
     C*
     C     #LODSFL       BegSr
     C                   Eval      *In(45) = *On
     C                   Write     SCR01C
     C                   Eval      *In(45) = *Off
     C                   Eval      W@Rrn1 = *Zeros
     C                   DoW       W@Rrn1  < @indx1
     C                   Eval      W@Rrn1  = W@Rrn1 + 1
     C*Load the array in subfile fields:
     C                   eval      scfnd  =  gp(@indx)
     C                   eval      scname =  nm(@indx)
     C                   eval      @indx  = @indx - 1
     C                   Write     SCR01S
     C                   EndDo
     C                   endsr
