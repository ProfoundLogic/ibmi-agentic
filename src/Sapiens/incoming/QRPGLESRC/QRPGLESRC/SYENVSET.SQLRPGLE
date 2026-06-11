     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) bnddir('STBNDDIR')
     H*====================================================================
     H/TITLE SYENVSET - Setup environment
     H*====================================================================
     H* - Mark the first column of the compile time array for copying
     H*   - 'Y' if it should be copied
     H*   - blank if it should not be copied
     H* - Update the from/to
     H*   - Library
     H*   - Co#
     H*   - Group
     H* - This program is currently configured for CLT150
     H*====================================================================
     H*  Created:  07/07/17  C:33  Project: 150WAB
     H*====================================================================
     H*  Revisions:
     H*
     H*====================================================================
     d#ofFiles         c                   220
     duseFile          s              1    dim(#ofFiles) ctdata perrcd(1)
     dfileInfo         s                   dim(#ofFiles) alt(useFile)
     d                                     like(dsFileInfo)

     ddsFileInfo       ds                  qualified
     d file                          10
     d fieldCo#                      10
     d fieldGrp                      10
     dsqlCo            s            500
     dsqlGrp           s            500
     dsqlSet           s            500
     dsqlStmt          s            500
     dsqlWhere         s            500
     di1               s              5  0

     dstrCoFrom        s              3    inz('777')
     dstrCoTo          s              3    inz('777')
     dstrGrpFrom       s              3    inz('900')
     dstrGrpTo         s              3    inz('999')
     dstrLibFrom       s             10    inz('PDSFIL099')
     dstrLibTo         s             10    inz('PDSFIL950')

     C*
      /Free
       For i1 = 1 to #ofFiles;
          If useFile(i1) <> 'Y';
             Iter;
          Endif;
          dsFileInfo = fileInfo(i1);
          Clear sqlWhere;
          Clear sqlSet;
          If dsFileInfo.fieldCo# <> *blanks;
             sqlWhere = %trim(dsFileInfo.fieldCo#) + ' = ' + strCoFrom;
             sqlSet = %trim(dsFileInfo.fieldCo#) + ' = ' + strCoTo;
          Endif;
          If dsFileInfo.fieldCo# <> *blanks and dsFileInfo.fieldGrp <> *blanks;
             sqlWhere = %trim(sqlWhere) + ' and ';
             sqlSet = %trim(sqlSet) + ', ';
          Endif;
          If dsFileInfo.fieldGrp <> *blanks;
             sqlWhere = %trim(sqlWhere) + ' ' +
              %trim(dsFileInfo.fieldGrp) + ' = ' + strGrpFrom;
             sqlSet = %trim(sqlSet) + ' ' +
              %trim(dsFileInfo.fieldGrp) + ' = ' + strGrpTo;
          Endif;
          If sqlWhere <> *blanks;
             sqlWhere = 'where ' + %trim(sqlWhere);
          Endif;
          If sqlSet <> *blanks;
             sqlSet = 'set ' + %trim(sqlSet);
          Endif;
          sqlStmt = 'insert into ' +
           %trim(strLibTo) + '/' + %trim(dsFileInfo.file) +
           ' (select * from ' +
           %trim(strLibFrom) + '/' + %trim(dsFileInfo.file) + ' ' +
           %trim(sqlWhere) + ')' +
           ' with NC';
          Exec Sql Execute IMMEDIATE :sqlStmt;
          Select;
          When strGrpFrom = strGrpTo and strCoFrom = strCoTo;
             Iter;
          Endsl;
          If sqlSet <> *blanks;
             sqlStmt = 'update ' +
              %trim(strLibTo) + '/' + %trim(dsFileInfo.file) + ' ' +
              %trim(sqlSet) + ' ' +
              %trim(sqlWhere) + ' ' +
              ' with NC';
             Exec Sql Execute IMMEDIATE :sqlStmt;
          Endif;
       Endfor;

       *inlr = *on;
      /end-free
**         Co#       Group
 FDF2P     F2CO#     F2FND
 FDF3P     F3CO#     F3FND
 FMCXP     CXCO#     CXFND
 FMFNP     FNCO#     FNFND
 FMFRP     FRCO#     FRFND
 FMF1P     F1CO#     F1FND
 FTGOP     GOCO#     GOFND
 WACLP     CLCO#     CLFND
 WAMLP     MLCO#     MLFND
YWDCDP     CDCO#     CDFND
YWDECP     ECCO#     ECFND
YWDFHP     FHCO#     FHFND
YWDF1P     F1CO#     F1FND
YWDF2P     F2CO#     F2FND
YWDF3P     F3CO#     F3FND
 WDHEP     HECO#     HEFND
 WDRBP     RBCO#     RBFND
YWDRCP     RCCO#     RCFND
YWDRRP     RRCO#     RRFND
 WDR1P     R1CO#     R1FND
YWDSCP     SCCO#     SCFND
YWDSTP     STCO#     STFND
YWDXSP     XSCO#     XSFND
 WMAAP     AACO#     AAFND
YWMADP     ADCO#     ADFND
YWMARP     ARCO#     ARFND
YWMAXP     AXCO#     AXFND
YWMBIP     BICO#     BIFND
YWMBXP     BXCO#     SXFND
YWMCFP     CFCO#     CFFND
 WMCIP     CICO#     CIFND
YWMCPP     CPCO#     CPFND
 WMCXP     CXCO#     CXFND
YWMDCP     DCCO#     DCFND
YWMDSP     DSCO#     DSFND
YWMDXP     DXCO#     DXFND
YWMEXP     EXCO#     EXFND
YWMFCP     FCCO#     FCFND
YWMFNP     FNNO#     FNFND
YWMGCP     GCCO#     GCFND
YWMGRP     GRCO#     GRFND
YWMIFP     IFCO#     IFFND
 WMMGP     MGCO#     MGFND
YWMNXP     NXCO#     NXFND
 WMPCP     PCCO#     PCFND
YWMPEP     PECO#     PEFND
YWMPFP     PFCO#     PFFND
YWMREP     RECO#     REFND
YWMRTP     RTCO#     RTFND
YWMSTP     STCO#     STFND
YWTC3P     C3CO#     C3FND
YWTDDP     DDCO#     DDFND
YWTENP     ENCO#     ENFND
YWTFIP     FICO#     FIFND
YWTLMP     LMCO#     LMFND
YWTMYP     MYCO#     MYFND
YWTXCP     XCCO#     XCFND
YIMCVP
YIMDIP
YIMFBP
YIMLBP
YIMOCP
YIMPSP
YIMTSP
YIMWSP
YITBSP
YITDSP
 MMOPP
 NMBDP
YSDRPF
YSMBUP
YSMCGP
YSMCOP     COCO#
YSMDEP     DECO#
YSMDOP     DOCO#
YSMFCP     FCCO#
YSMKDP
YSMKTP
YSMOCP
YSMORP
YSMVDP
YSSYPF
YSTATE
YSTCTP
YSTGIP     GOCO#
YSTGTP     GTCO#
YSTSMP
YSTSTP
YSWCPF
YWMDEP     DECO#
YWMDOP     DOCO#
YWMOFP     OFCO#
YWMPRP     PRCO#
YWMRSP     RSCO#
YWTACP     ACCO#
YWTASP     ASCO#
YWTATP     ATCO#
YWTAVP     AVCO#
 WTBAP     BACO#
YWTBCP     BCCO#
YWTBSP     BSCO#
 WTB2P     B2CO#
 WTB7P     B7CO#
 WTCAP     CACO#
YWTCNP     CNCO#
YWTCRP     CRCO#
YWTCSP     CSCO#
YWTCTP     CTCO#
 WTDCP     DCCO#
 WTEMP     EMCO#
 WTERP     ERCO#
YWTGCP     GCCO#
YWTGIP     GICO#
YWTGTP     GTCO#
 WTIDP     IDCO#
 WTISP     ISCO#
 WTLRP     LRCO#
 WTNAP     NACO#
YWTNCP     NCCO#
YWTOCP     OCCO#
 WTPAP     PACO#
 WTPCP     PCCO#
YWTPOP     POCO#
YWTPSP     PSCO#
YWTQUP     QUCO#
 WTRCP     RCCO#
YWTRSP     RSCO#
 WTRTP     RTCO#
YWTSAP     SACO#
 WTSPP     SPCO#
YWTSYP     SYCO#
YWTTTP     TTCO#
YSTAPP
YSTDEP     DECO#
YSTDLP     DLCO#
YSTDRP
YSTMCP
YSTOPP     OPCO#
YSTPRP
YSTP1P
YSTP2P
 STRMP     RMCO#
YSTSCP
 STSFP
YSTSMP
YSTSPP
YSTSP1P
YSTSSP     SSCO#
YSTTBP
YWTAAP     AACO#
YWTARP     ARCO#
YWTASP     ASCO#
YWTATP     ATCO#
YWTAVP     AVCO#
YWTAYP     AYCO#
YWTBAP     BACO#
YWTBCP     BCCO#
YWTBSP     BSCO#
YWTB3P     B3CO#
YWTCBP     CBCO#
YWTCCP     CCCO#
YWTCDP     CDCO#
YWTCIP     CICO#
YWTCPP
YWTCVP     CVCO#
YWTC1P     C1CO#
YWTC2P     C2CO#
YWTDAP     DACO#
YWTDEP     DECO#
YWTDMP     DMCO#
YWTDSP     DSCO#
YWTDVP1    DVCO#
YWTECP     ECCO#
YWTGSP
YWTHYP     HYCO#
YWTIDP     IDCO#
YWTISP     ISCO#
YWTMFP     MFCO#
YWTNAP     NACO#
YWTNDP     NDCO#
YWTOCP     OCCO#
YWTQSP     QSCO#
YWTRCP     RCCO#
YWTRSP     RSCO#
YWTRTP     RTCO#
YWTSCP     SCCO#
YWTSFP     SFCO#
YWTSSELP
YWTSSP     SSCO#
YWTSTP     STCO#
YWTTAP     TACO#
YWTTBP     TBCO#
YWTTGP     TGCO#
YWTTRP     TRCO#
YWTVIP     VICO#
YWTVLP     VLCO#
YWTVSP     VSCO#
YWTXCP     XCCO#
YWTXFP
