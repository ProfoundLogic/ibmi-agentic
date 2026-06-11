    H*  (C) Copyright Progressive Data Solutions, Inc., 2016
    H*********************************************************************
    H/TITLE SC954R - DARTS deletion remarks
    H*********************************************************************
    H*
    H* CREATED:  02/18/16     C:53     PROJECT: 114IWR
    H*********************************************************************
    H* REVISIONS:
    H*
    H* 00/00/00 C:000 Project:
    H*
    H*********************************************************************
      * Darts Master file
     fsdcmle    uf   e           k disk    rename(pcm01:pcmle)
      * Additional Remarks file
     fsmarl1    uf   e           k disk
      * Darts Additional Information File
     fsdail     uf   e           k disk
      * Document Location Record file
     fsmdll2    uf   e           k disk
      * Document Object Transaction file
     fimdolC    uf   e           k disk    rename(pdo01:pdolC)
      * Document Object Transaction file
     fimdolD    if   e           k disk    rename(pdo01:pdolD) prefix(ld)
      * Document Object Transaction file
     fimdolA    uf   e           k disk    rename(pdo011:pdolA) prefix(la)
      * Multi object file
     fsmmdl     uf   e           k disk    rename(pmd01:pmdl0)
      * DARTS transfer history
     fsxdtp     o  a e           k disk
      *
     d qqkey           s                   like(cmkey)
     d qqseq           s                   like(cmseq#)
     d qqCall          s              1
     d doccnt          s             12  0
     d ##fkey          s                   like(cmkey)
     d ##fseq          s                   like(cmseq#)
     d ##tkey          s                   like(cmkey)
     d ##tseq          s                   like(cmseq#)
     d ##call          s              1
      *
     c     *entry        plist
     c                   parm                    qqkey
     c                   parm                    qqseq
     c                   parm                    qqCall
      *
     c     key           klist
     c                   kfld                    qqkey
     c                   kfld                    qqseq
      *
     c     keyOne        klist
     c                   kfld                    CMkey
     c                   kfld                    CMseq#
      *
     c     keyAll        klist
     c                   kfld                    qqkey
      *
     c     keydoc        klist
     c                   kfld                    dodcid
      *
      *========================================================================
      *  SMARP - Delete Additional Remarks Records
      *
     c                   dou       *in60
     c                   If        qqCall = 'A'
     c     keyAll        delete    par01                              60
     c                   Else
     c     key           delete    par01                              60
     c                   EndIf
     c                   enddo
      *
      *  SMDLP - Delete Document Location Record (For optical location only)
      *
     c                   dou       *in60
     c                   If        qqCall = 'A'
     c     keyAll        delete    pdl01                              60
     c                   Else
     c     key           delete    pdl01                              60
     c                   EndIf
     c                   enddo
      *
      *========================================================================
      * Process SDCMP
     c                   If        qqCall = 'A'
     c     keyAll        setll     pcmle
     c     keyAll        reade     pcmle
     c                   Else
     c     key           setll     pcmle
     c     key           reade     pcmle
     c                   EndIf

     c                   dow       not %eof(sdcmle)
      *
      * IMDOP, IMDOP1 - Delete Image records.
      *
     c     keyOne        chain     pdolC
     c                   eval      doccnt = 0
     c     keydoc        setll     pdolD
     c     keydoc        reade     pdolD
     c                   dow       not(%eof)
     c                   eval      doccnt = doccnt + 1
     c     keydoc        reade     pdolD
     c                   enddo
      *
     c     keyOne        delete    pdolA
      *
     c     doccnt        ifeq      1
     c     keyOne        delete    pdolC
     c                   endif
      *
      *  SDAIP - Delete additional info records
      *
     c                   dou       *in60
     c     keyOne        delete    pai01                              60
     c                   enddo
      *
      *  SMCLP - Delete CLOB records
      *
     c                   call      'DA703R'
     c                   parm      CMkey         ##fkey
     c                   parm      CMseq#        ##fseq
     c                   parm      *blanks       ##tkey
     c                   parm      *zeros        ##tseq
     c                   parm      'D'           ##call

      *** Write history record
     c                   clear                   pdt01
     c                   eval      dtfrky = cmkey
     c                   eval      dtfrdd = cmcdat
     c                   eval      dtfrs# = cmseq#
     c                   eval      dtfrrc = cmrmcd
     c                   eval      dtfrdi = cmdifl
     c                   eval      dtfrdu = cmuser

     c                   eval      dttoky = 'DELETED'
     c                   eval      dttodd = 0
     c                   eval      dttos# = 0
     c                   eval      dttorc = *blanks
     c                   eval      dttodi = *blanks
     c                   eval      dttodu = *blanks
     c                   write     pdt01

      *** Delete main record
     c                   delete    pcmle

     c                   If        qqCall = 'A'
     c     keyAll        reade     pcmle
     c                   Else
     c     key           reade     pcmle
     c                   EndIf
      * enddo for pcmle/sdcmle
     c                   enddo
      *
     c                   Return
