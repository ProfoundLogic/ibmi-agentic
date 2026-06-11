     H*  (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 1998
     H*
     H/TITLE CVT63CBJ
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    CREATE A TABLE ENTRY FOR ALL THE NEW COMPANIES CREATED FOR *
     H*    PCMS                                                       *
     H*                                                               *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  04/28/06 C:168  PROJECT:63CBJ                                *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
      * Policy files
     Fcaa1l     uF A E           K DISK
     Fcmbml     uF A E           K DISK
     Fcmgll     uF A E           K DISK
     Fcaptl     uF A E           K DISK
     Fcmbol     uF A E           K DISK
     Fcmiml     uF A E           K DISK
     Fcdarl     uF A E           K DISK
     Fcmbtl     uF A E           K DISK
     Fcmlll     uF A E           K DISK
     Fcdgil     uF A E           K DISK
     Fcmdrl     uF A E           K DISK
     Fcmlol     uF A E           K DISK
     Fcdiil     uF A E           K DISK
     Fcmenl     uF A E           K DISK
     Fcmpal     uF A E           K DISK
     Fcmail     uF A E           K DISK
     Fcmfal     uF A E           K DISK
     Fcmpml     uF A E           K DISK
     Fcmasl     uF A E           K DISK
     Fcmfil     uF A E           K DISK
     Fcmpxl     uF A E           K DISK
     Fcmaul     uF A E           K DISK
     Fcmfrl     uF A E           K DISK
     Fcmp1l     uF A E           K DISK    rename(ppc01:ppc02)
     Fcma1l     uF A E           K DISK    rename(pa101:pa102)
     Fcmgal     uF A E           K DISK
     Fcmvsl     uF A E           K DISK
     Fcmbll     uF A E           K DISK
      *
      * Claim files
     Fcarpl     uF A E           K DISK
     Fcmcml     uF A E           K DISK
     Fcdcbl     uF A E           K DISK
     Fcmrrl     uF A E           K DISK
     Fcdcdl     uF A E           K DISK
     Fcmsul     uF A E           K DISK
     F*cdcil     uF A E           K DISK
     Fcdcsl     uF A E           K DISK
     Fcdpal     uF A E           K DISK    rename(ppa01:ppa02)
     Fcdrhl     uF A E           K DISK
     Fcdrpl     uF A E           K DISK    rename(prp01:prp02)
     Fcmcll     uF A E           K DISK
     F*
     F*
     C     *LIKE         DEFINE    PMCO#         XXCO#
     C     *LIKE         DEFINE    PMPOL#        XXPOL#
     C     *LIKE         DEFINE    RPCLM#        XXCLM#
     C     *LIKE         DEFINE    pmco#         xxnew#
     C*
     C*
     C     POLKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXPOL#
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    cmclm#
     C*
     C*
     C*  Get the policy record
     C*
     C                   EVAL      XXCO# = 393
     C                   EVAL      XXPOL# = 'W H BRAUM INC'
     C                   EVAL      xxnew# = 7
     C     polky         setll     CMPML
     C     POLKY         reade     CMPML
     C                   DOW       NOT %EOF(CMPML)
     c                   exsr      @chgpol
     c                   exsr      @chgclm
     C                   EVAL      pmCO# = xxnew#
     C                   update    ppm01
     C     polky         READE     cmpml
     C                   ENDDO
      *
     C                   EVAL      XXPOL# = 'WHB TRANSPORT'
     C                   EVAL      xxnew# = 13
     C     POLKY         setll     CMPML
     C     POLKY         reade     CMPML
     C                   DOW       NOT %EOF(CMPML)
     c                   exsr      @chgpol
     c                   exsr      @chgclm
     C                   EVAL      pmCO# = xxnew#
     C                   update    ppm01
     C     polky         READE     cmpml
     C                   ENDDO
      *
     C*
     C                   SETON                                        LR
     c                   return
      **********************************************************************
      *                                                                    *
      * Change all the records for the policy files                        *
      *                                                                    *
      **********************************************************************
     c     @chgpol       begsr
      *
      *
      * File ( CAAIP )
      *
     C     polky         CHAIN     caa1l
     C                   DOW       not %eof(caa1l)
     C                   EVAL      aiCO# = xxnew#
     C                   update    pa101
     C     polky         READE     caa1l
     C                   ENDDO
      *
      * File ( cmbmp )
      *
     C     polky         CHAIN     cmbml
     C                   DOW       not %eof(cmbml)
     C                   EVAL      bmCO# = xxnew#
     C                   update    pbm01
     C     polky         READE     cmbml
     C                   ENDDO
      *
      * File ( cmglp )
      *
     C     polky         CHAIN     cmgll
     C                   DOW       not %eof(cmgll)
     C                   EVAL      glCO# = xxnew#
     C                   update    pgl01
     C     polky         READE     cmgll
     C                   ENDDO
      *
      * File ( captp )
      *
     C     polky         CHAIN     captl
     C                   DOW       not %eof(captl)
     C                   EVAL      ptcO# = xxnew#
     C                   update    ppt01
     C     polky         READE     captl
     C                   ENDDO
      *
      * File ( cmbop )
      *
     C     polky         CHAIN     cmbol
     C                   DOW       not %eof(cmbol)
     C                   EVAL      boCO# = xxnew#
     C                   update    pbo01
     C     polky         READE     cmbol
     C                   ENDDO
      *
      * File ( cmimp )
      *
     C     polky         CHAIN     cmiml
     C                   DOW       not %eof(cmiml)
     C                   EVAL      imCO# = xxnew#
     C                   update    pim01
     C     polky         READE     cmiml
     C                   ENDDO
      *
      * File ( cdarp )
      *
     C     polky         CHAIN     cdarl
     C                   DOW       not %eof(cdarl)
     C                   EVAL      arCO# = xxnew#
     C                   update    par01
     C     polky         READE     cdarl
     C                   ENDDO
      *
      * File ( cmbtp )
      *
     C     polky         CHAIN     cmbtl
     C                   DOW       not %eof(cmbtl)
     C                   EVAL      btCO# = xxnew#
     C                   update    pbt01
     C     polky         READE     cmbtl
     C                   ENDDO
      *
      * File ( cmllp )
      *
     C     polky         CHAIN     cmlll
     C                   DOW       not %eof(cmlll)
     C                   EVAL      llCO# = xxnew#
     C                   update    pll01
     C     polky         READE     cmlll
     C                   ENDDO
      *
      * File ( cdgip )
      *
     C     polky         CHAIN     cdgil
     C                   DOW       not %eof(cdgil)
     C                   EVAL      giCO# = xxnew#
     C                   update    pgi01
     C     polky         READE     cdgil
     C                   ENDDO
      *
      * File ( cmdrp )
      *
     C     polky         CHAIN     cmdrl
     C                   DOW       not %eof(cmdrl)
     C                   EVAL      drCO# = xxnew#
     C                   update    pdr01
     C     polky         READE     cmdrl
     C                   ENDDO
      *
      * File ( cmlop )
      *
     C     polky         CHAIN     cmlol
     C                   DOW       not %eof(cmlol)
     C                   EVAL      loCO# = xxnew#
     C                   update    plo01
     C     polky         READE     cmlol
     C                   ENDDO
      *
      * File ( cdiip )
      *
     C     polky         CHAIN     cdiil
     C                   DOW       not %eof(cdiil)
     C                   EVAL      iiCO# = xxnew#
     C                   update    pii01
     C     polky         READE     cdiil
     C                   ENDDO
      *
      * File ( cmenp )
      *
     C     polky         CHAIN     cmenl
     C                   DOW       not %eof(cmenl)
     C                   EVAL      enCO# = xxnew#
     C                   update    pen01
     C     polky         READE     cmenl
     C                   ENDDO
      *
      * File ( cmpap )
      *
     C     polky         CHAIN     cmpal
     C                   DOW       not %eof(cmpal)
     C                   EVAL      paCO# = xxnew#
     C                   update    ppa01
     C     polky         READE     cmpal
     C                   ENDDO
      *
      * File ( cmaip )
      *
     C     polky         CHAIN     cmail
     C                   DOW       not %eof(cmail)
     C                   EVAL      aiCO# = xxnew#
     C                   update    pai01
     C     polky         READE     cmail
     C                   ENDDO
      *
      * File ( cmfap )
      *
     C     polky         CHAIN     cmfal
     C                   DOW       not %eof(cmfal)
     C                   EVAL      faCO# = xxnew#
     C                   update    pfa01
     C     polky         READE     cmfal
     C                   ENDDO
      *
      * File ( cmasp )
      *
     C     polky         CHAIN     cmasl
     C                   DOW       not %eof(cmasl)
     C                   EVAL      asCO# = xxnew#
     C                   update    pas01
     C     polky         READE     cmasl
     C                   ENDDO
      *
      * File ( cmfip )
      *
     C     polky         CHAIN     cmfil
     C                   DOW       not %eof(cmfil)
     C                   EVAL      fiCO# = xxnew#
     C                   update    pfi01
     C     polky         READE     cmfil
     C                   ENDDO
      *
      * File ( cmpxp )
      *
     C     polky         CHAIN     cmpxl
     C                   DOW       not %eof(cmpxl)
     C                   EVAL      pxCO# = xxnew#
     C                   update    ppx01
     C     polky         READE     cmpxl
     C                   ENDDO
      *
      * File ( cmaup )
      *
     C     polky         CHAIN     cmaul
     C                   DOW       not %eof(cmaul)
     C                   EVAL      auCO# = xxnew#
     C                   update    pau01
     C     polky         READE     cmaul
     C                   ENDDO
      *
      * File ( cmfrp )
      *
     C     polky         CHAIN     cmfrl
     C                   DOW       not %eof(cmfrl)
     C                   EVAL      frCO# = xxnew#
     C                   update    pfr01
     C     polky         READE     cmfrl
     C                   ENDDO
      *
      * File ( cmp1p )
      *
     C     polky         CHAIN     cmp1l
     C                   DOW       not %eof(cmp1l)
     C                   EVAL      pcCO# = xxnew#
     C                   update    ppc02
     C     polky         READE     cmp1l
     C                   ENDDO
      *
      * File ( cma1p )
      *
     C     polky         CHAIN     cma1l
     C                   DOW       not %eof(cma1l)
     C                   EVAL      a1CO# = xxnew#
     C                   update    pa102
     C     polky         READE     cma1l
     C                   ENDDO
      *
      * File ( cmgap )
      *
     C     polky         CHAIN     cmgal
     C                   DOW       not %eof(cmgal)
     C                   EVAL      gaCO# = xxnew#
     C                   update    pga01
     C     polky         READE     cmgal
     C                   ENDDO
      *
      * File ( cmvsp )
      *
     C     polky         CHAIN     cmvsl
     C                   DOW       not %eof(cmvsl)
     C                   EVAL      vsCO# = xxnew#
     C                   update    pvs01
     C     polky         READE     cmvsl
     C                   ENDDO
      *
      * File ( cmblp )
      *
     C     polky         CHAIN     cmbll
     C                   DOW       not %eof(cmbll)
     C                   EVAL      blCO# = xxnew#
     C                   update    pbl01
     C     polky         READE     cmbll
     C                   ENDDO
      *
     c                   endsr
      **********************************************************************
      *                                                                    *
      * Change all the records for the claims files                        *
      *                                                                    *
      **********************************************************************
     c     @chgclm       begsr
      *
     C     POLKY         CHAIN     CMCML
     C                   DOW       NOT %EOF(CMCML)
      *
      *
      * File ( carpp )
      *
     C     cmky          CHAIN     carpl
     C                   DOW       not %eof(carpl)
     C                   EVAL      arCO# = xxnew#
     C                   update    prp01
     C     cmky          READE     carpl
     C                   ENDDO
      *
      * File ( cdcbp )
      *
     C     cmky          CHAIN     cdcbl
     C                   DOW       not %eof(cdcbl)
     C                   EVAL      cbCO# = xxnew#
     C                   update    pcb01
     C     cmky          READE     cdcbl
     C                   ENDDO
      *
      * File ( cmrrp )
      *
     C     cmky          CHAIN     cmrrl
     C                   DOW       not %eof(cmrrl)
     C                   EVAL      rrCO# = xxnew#
     C                   update    prr01
     C     cmky          READE     cmrrl
     C                   ENDDO
      *
      * File ( cdcdp )
      *
     C     cmky          CHAIN     cdcdl
     C                   DOW       not %eof(cdcdl)
     C                   EVAL      cdCO# = xxnew#
     C                   update    pcd01
     C     cmky          READE     cdcdl
     C                   ENDDO
      *
      * File ( cmsup )
      *
     C     cmky          CHAIN     cmsul
     C                   DOW       not %eof(cmsul)
     C                   EVAL      suCO# = xxnew#
     C                   update    psu01
     C     cmky          READE     cmsul
     C                   ENDDO
      *
      * File ( cdcip )
      *
     C**   cmky          CHAIN     cdcil
     C**                 DOW       not %eof(cdcil)
     C**                 EVAL      ciCO# = xxnew#
     C**                 update    pci01
     C**   cmky          READE     cdcil
     C**                 ENDDO
      *
      * File ( cdcsp )
      *
     C     cmky          CHAIN     cdcsl
     C                   DOW       not %eof(cdcsl)
     C                   EVAL      csCO# = xxnew#
     C                   update    pcs01
     C     cmky          READE     cdcsl
     C                   ENDDO
      *
      * File ( cdpap )
      *
     C     cmky          CHAIN     cdpal
     C                   DOW       not %eof(cdpal)
     C                   EVAL      paCO# = xxnew#
     C                   update    ppa02
     C     cmky          READE     cdpal
     C                   ENDDO
      *
      * File ( cdrhp )
      *
     C     cmky          CHAIN     cdrhl
     C                   DOW       not %eof(cdrhl)
     C                   EVAL      rhCO# = xxnew#
     C                   update    prh01
     C     cmky          READE     cdrhl
     C                   ENDDO
      *
      * File ( cdrpp )
      *
     C     cmky          CHAIN     cdrpl
     C                   DOW       not %eof(cdrpl)
     C                   EVAL      rpCO# = xxnew#
     C                   update    prp02
     C     cmky          READE     cdrpl
     C                   ENDDO
      *
      * File ( cmclp )
      *
     C     cmky          CHAIN     cmcll
     C                   DOW       not %eof(cmcll)
     C                   EVAL      clCO# = xxnew#
     C                   update    pcl01
     C     cmky          READE     cmcll
     C                   ENDDO
      *
     C                   EVAL      cmCO# = xxnew#
     c                   update    pcm01
     C     polky         reade     CMCML
     c                   enddo
      *
     c                   endsr
