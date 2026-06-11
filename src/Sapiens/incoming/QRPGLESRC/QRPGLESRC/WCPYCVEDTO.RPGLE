      * (C) Copyright Progressive Data Solutions, Inc., 2005                                       *
      *=============================================================================================
      *  CREATED 11/11/05   C:136   PROJECT: 136WBD                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * xx/xx/xx  C:xxx Project: xxxxx                                                             *
      *=============================================================================================
     Oqsysprt   e            head           1 06
     O                       sysTimeUSA
     O                       companyNm          120
     O                       sysDateUSA         180
     O          e            head           2
     O                       q1pgm
     O                       rptTitle           120
     O                                          174 'PAGE:'
     O                       page          z    180
     O          e            detHeader      0
     O                       line
     O          e            detHeader      2
     O                       keyValues           30
     O                                           43 'Error Codes'
     O          e            detail         1
     O                       keyField            30
     O                       errCodes           180
     O          e            errHead        2
     O                                          100 'Error Summary Listing'
     O          e            errHead        0
     O                       line
     O          e            errHead        2
     O                                            5 'Field'
     O                                           24 'Error Code'
     O                                           38 'Error Count'
     O                                           58 'Error Description'
     O          e            errDetail      1
     O                       svField             10
     O                       svCode              24
     O                       errCount      1     38
     O                       errDescription     101
     O          e            errTotal    2  1
     O                                              'Number of records:'
     O                       recCount      1     29
     O          e            errTotal       1
     O                                              'Number of errors :'
     O                       errTtlCnt     1     29
