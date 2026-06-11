     * (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 2005
     *********************************************************************
     *TITLE FIXWMPXP  - LOAD EFFECTIVE DATE TO WMPXP
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     *********************************************************************
     *  DESCRIPTION:  THIS PROGRAM WILL DRIVE ACROSS THE PAYROLL MASTER  *
     *                FILE. FOR EACH CO#/RTCD/CLAS/EFDT IN WMPRP, A      *
     *                WMPXP RECORD WILL BE WRITTEN WITH THAT EFDT        *
     *                PROVIDING THAT A WMPXP RECORD EXISTS WITH THE      *
     *                CORRESPONDING CO#/CLAS/ST INFO. IN ADDITION,       *
     *                IF A WMPXP RECORD WITH A BLANK STATE VALUE EXISTS  *
     *                FOR THAT PAYROLL CLASS, A RECORD WILL BE WRITTEN   *
     *                WITH THAT EFDT ALSO.                               *
     *                                                                   *
     *  CALLED FROM:                                                     *
     *    COMMAND LINE                                                   *
     *  CALLS TO:                                                        *
     *                                                                   *
     *  CREATED: 06/20/05  PROGRAMMER: C:136  PROJ: WC945                *
     *********************************************************************
     * REVISIONS:                                                        *
     *                                                                   *
     *********************************************************************
     *
     fWMPRL     if   e           k disk
     fWMRSL     if   e           k disk
     *
     fWMPXL2    uf a e           k disk    rename(ppx01:ppx0l2)
     fWMPXL3    uf   e           k disk    rename(ppx01:ppx0l3)
     *
    
     c     rsky          klist
     c                   kfld                    PRCO#
     c                   kfld                    PRRTCD
    
     c     pxky          klist
     c                   kfld                    PRCO#
     c                   kfld                    PRCLAS
     c                   kfld                    kkst              2
    
     c     pxky1         klist
     c                   kfld                    PRCO#
     c                   kfld                    PRCLAS
     c                   kfld                    kkst
     c                   kfld                    PXEXT
    
     c     pxky2         klist
     c                   kfld                    PRCO#
     c                   kfld                    PRCLAS
     c                   kfld                    PREFDT
     c                   kfld                    kkst
     c                   kfld                    PXEXT
    
     c                   read      ppr01
     c                   dow       not(%eof)
    
     c     rsky          chain     prs01
     c                   if        %found
     c                   eval      kkst = RSJRST
     c                   else
     c                   eval      kkst = *hival
     c                   endif

     c     pxky          setll     ppx0l3
     c     pxky          reade     ppx0l3
     c                   dow       not(%eof)

     c                   if        PXEFDT = 0
     c                   eval      PXEFDT = PREFDT
     c                   update    ppx0l3
     c                   else

     c     pxky2         chain(n)  ppx0l2
     c                   if        not(%found)
     c                   eval      PXEFDT = PREFDT
     c                   write     ppx0l2
     c                   endif

     c     pxky1         setgt     ppx0l3
     c                   endif

     c     pxky          reade     ppx0l3
     c                   enddo
    
     c                   eval      kkst = *blanks
     c     pxky          setll     ppx0l3
     c     pxky          reade     ppx0l3
     c                   dow       not(%eof)

     c                   if        PXEFDT = 0
     c                   eval      PXEFDT = PREFDT
     c                   update    ppx0l3
     c                   else

     c     pxky2         chain(n)  ppx0l2
     c                   if        not(%found)
     c                   eval      PXEFDT = PREFDT
     c                   write     ppx0l2
     c                   endif

     c     pxky1         setgt     ppx0l3
     c                   endif

     c     pxky          reade     ppx0l3
     c                   enddo
    
     c                   read      ppr01
     c                   enddo
    
    c                   seton                                        lr
