     D*=====================================================================
     D* Producer processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* GetProdNm - Get producer name
    P*
    P*  GetProdNm - returns producer name
    P*   fein      - accepts the agent fein to search over (required)
    P*   mod       - accepts the agent mod to search over  (required)
    P*   prod#     - passed in for validation              (required)
    P*   ap_rrn    - returns the RRN of WMAPP
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     DGetProdNm        PR            50
     D fein                           9  0 const
     D mod                            3  0 const
     D prod#                          5  0 const
     D ap_rrn                         9  0              options(*nopass)
     D co#                            3  0 const        options(*nopass)
    P*
    P*====================================================================
    P* GetProdRRN - Get producer RRN
    P*
    P*  GetProdRRN - returns the RRN of WMAPP
    P*   fein     - passed in for validation  (required)
    P*   mod      - passed in for validation  (required)
    P*   prod#    - passed in for validation  (required)
    P*   co#      - passed in for validation
    P*====================================================================
    P*
     DGetProdRRN       PR             9  0
     D fein                           9  0                  const
     D mod                            3  0                  const
     D prod#                          5  0                  const
     D co#                            3  0 options(*nopass) const
    P*
    P*====================================================================
    P* Validate the producer is associated with a state
    P*
    P*  ValidStateForProducer - returns *on if the state is valid
    P*   co#      - passed in for validation  (required)
    P*   fnd      - passed in for validation  (required)
    P*   agt#     - passed in for validation  (required)
    P*   prod     - passed in for validation  (required)
    P*   code     - passed in for validation  (required) this can be state or rate set code
     P*   type     - passed in for validation  (optional) {State/RateSet, defaults to state}
    P*====================================================================
     dValidStateForProducer...
     d                 pr             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inAgt#                         5  0 const
     d inProd                         5  0 const
     d ioCode                         3
     d inType                        10    const varying options(*nopass)
