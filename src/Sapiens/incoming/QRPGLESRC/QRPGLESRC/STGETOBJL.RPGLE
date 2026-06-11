     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE STGETOBJL - AJAX request handler to serve library name
     H*****************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Libary nam AJAX request hadler          *
     H*                                                               *
     H* CALLED BY: AJAX request thru HTTP server                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 10/10/12  C:191 PROJECT: Transform                   *
     H*                                                               *
     H*  To compile program:                                          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************

     DGetObjLibrary    pr                  ExtPgm('STGETOBJL')
     D                              512
     D                            32767
     D                                1
      *
     DGetObjLibrary    pi
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     DRtvObjLib        pr                  ExtPgm('RTVOBJLIB')
     D inObjName                     10
     D inObjType                      7
     D outLibrary                    10

      /copy sPrAtrium
      *
     D xxObjName       s             10    inz
     D xxObjType       s              7    inz
     D xxLibrary       s             10    inz

      /Free
       outJSON = ' ';
       xxObjName = 'STLAUNCH';
       xxObjType = '*PGM';
       xxLibrary = *Blanks;
       Monitor;
         RtvObjLib(xxObjName:xxObjType:xxLibrary);
       On-Error;
         xxLibrary = *Blanks;
       EndMon;
       BuildJSON(outJSON :'ObjectLib' :%Trim(xxLibrary));
       RtnCode = ' ';
       Return;
      /End-free
