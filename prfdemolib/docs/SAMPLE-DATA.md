# PRFDEMOLIB sample data: what to key in

These screens have no working **F4 prompt / search** functions; the lookup programs are not part of the demo. Use this sheet instead. Every value below was read from library `AITSK00095` by `prfdemolib/tools/gen-sample-data-doc.py`, so it matches what the screens show. Regenerate it after reloading data.

**Where the data comes from:** 186 SKUs in total. 162 came from PRFDEMOLIB (a real sample from Variety Wholesalers / Roses). 24 were **seeded for the demo** (SKUs 8801xx Seasonal Candy and 8802xx Bedding, marked *seed* below), modelled on what Roses sells, because the shipped sample is concentrated in one department. The seeding also added the four **inventory groups**, which were empty, and five vendors. See [How the data was built](#how-the-data-was-built).

Contents: [Try these first](#try-these-first) · [Stores and warehouses](#stores-and-warehouses) · [Regions, districts, groups](#regions-districts-and-groups) · [Seasons](#seasons) · [Departments](#departments) · [SKU catalog](#sku-catalog) · [Vendors](#vendors)

## Try these first

### INV046: Inventory by Store (menu option 4)

| SKU Number | Store Type | Search For | Number | What you see |
|---|---|---|---|---|
| 262657 | A | G | 740 | Store group SOFA TEST STORES: all 10 stores, group and chain totals |
| 880103 | A | I | 30 | Inventory group MISSISSIPPI STORES (205, 262): peppermint candy canes |
| 880209 | A | I | 10 | NC & VA STORES (406, 458, 1838): sherpa throws |
| 100001 | S | R | 3 | Region 3 (406, 535, 1838): a fast-selling washcloth |
| 827412 | A | D | 11 | District 11 (store 205 only) |
| 880101 | A | S | 205 | Starts at store 205 and lists onward, including the DCs |
| 880110 | W | S | 9001 | Warehouses only: 9001 onward |

Press **F5** to switch between current period and year to date, and **Page Down** for more stores.

> **Use Store Type `A` with Search For `G` or `I`.** Every group member is a store, so the result is the same, and `S`/`W` are unreliable there: INV046 checks the store type of the *previous* store it read (an original program bug), so on the first inquiry the list can come back empty.

### INV502: Item Inquiry (menu option 5)

| SKU | What it shows |
|---|---|
| 262657 | ASST TOPS: large pack-and-hold at both DCs, next out 1/03/27 |
| 100001 | 13x13 WASH CLOTH: allocated quantities at 9001/9002, a busy replenishment item |
| 248117 | Sofa sold as a *Bundle Component* (Set Code 6) |
| 166402 | ASST SWIM SEPARATE TOPS: pack-and-hold |
| 880209 | SHERPA THROW (seed): new vendor, Fall/Winter 2026 season, pack-and-hold releasing 12/01/26 |
| 880103 | PEPPERMINT CANDY CANES (seed): holiday candy with pack-and-hold |
| 880206 | 7PC COMFORTER SET (seed): big-ticket item, $30.00 |
| 999999 | Error path: *INV0297: Invalid sku entered.* |

**Page Down / Page Up** step through every SKU in number order (186 of them).

### TS0110: Ticket Item Inventory (menu option 6)

| Store | Dept | Ticketed only | Available only | What you see |
|---|---|---|---|---|
| 205 | 12 | N | N | Ladies Intimate Basics, the full department (largest list) |
| 205 | 12 | Y | Y | The same department narrowed to ticketed items the DC can ship |
| 205 | 86 | Y | Y | Seasonal Candy (seed): Christmas and Harvest candy, pack-and-hold on candy canes |
| 406 | 86 | N | N | The same department from warehouse 9001 (store 406) |
| 205 | 59 | N | N | Bedding (seed): sheets, comforters, throws; includes unmapped (Map N) items |
| 458 | 59 | Y | Y | Bedding at a 9001-supplied store |
| 205 | 40 | N | N | A one-item department (ASST TOPS) |
| 999 | 12 |  |  | Error path: *Invalid store number - press RESET* |

Season: leave **ALL**, or key a season code from [Seasons](#seasons), e.g. `7` for Fall/Winter 2026 only. To try **F10 = create distribution**, key `1`-`3` in the **Pks** column on one or two lines first.

## Stores and warehouses

All stores are Roses stores. *Short name* is what INV046 shows in its Store column. *Warehouse* is the DC that supplies the store; TS0110 shows it top left (e.g. WH9002).

| Store | Short name | City, state | Region | District | Warehouse | Price zone |
|---|---|---|---|---|---|---|
| 188 | 188 ELIZ | Elizabethtown, KY | 2 | 25 | 9002 | 1 |
| 205 | 205 LAUR | Laurel, MS | 1 | 11 | 9002 | 1 |
| 215 | 215 MACO | Macon, GA | 5 | 51 | 9002 | 1 |
| 257 | 257 GRIF | Griffin, GA | 5 | 52 | 9002 | 1 |
| 262 | 262 JACK | Pearl, MS | 1 | 14 | 9002 | 1 |
| 406 | 406 SOUT | South Hill, VA | 3 | 34 | 9001 | 1 |
| 458 | 458 GRNS | Greensboro, NC | 2 | 20 | 9001 | 1 |
| 499 | 499JCKSN | Jacksonville, FL | 4 | 47 | 9002 | 1 |
| 535 | 535GLEN | Glen Burnie, MD | 3 | 38 | 9001 | 1 |
| 1838 | 1838FARM | Farmville, NC | 3 | 39 | 9001 | 1 |

**Warehouses / distribution centres** (Store Type `W` in INV046):

| Number | Name |
|---|---|
| 9001 | HENDERSON DIST CTR (REC) |
| 9002 | NEWNAN GA DIST CTR  (INV) |
| 9006 | HENDERSON RETURN CTR (INV) |
| 9007 | NEWNAN REPACK |
| 9151 | BLOTS HENDERSON DC |
| 9152 | BLOTS NEWNAN DC |

There are no offices (`O`) or inventory locations (`I`) in the sample, so those Store Type codes list nothing.

## Regions, districts and groups

For INV046 *Search For*:

| Search For | Number | Name | Stores |
|---|---|---|---|
| R | 1 | REGION 1 | 205, 262 |
| R | 2 | REGION 2 | 188, 458 |
| R | 3 | REGION 3 | 406, 535, 1838 |
| R | 4 | REGION 4 | 499 |
| R | 5 | REGION 5 | 215, 257 |
| D | 11 | Robert Smith (region 1) | 205 |
| D | 14 | David Hamilton (region 1) | 262 |
| D | 20 | Brian Wells (region 2) | 458 |
| D | 25 | James Wofford (region 2) | 188 |
| D | 34 | Sam Ray (region 3) | 406 |
| D | 38 | Eunice Jones (region 3) | 535 |
| D | 39 | Stacie Hassell (region 3) | 1838 |
| D | 47 | Thomas England (region 4) | 499 |
| D | 51 | Jimmy Moran (region 5) | 215 |
| D | 52 | Phil Auguste (region 5) | 257 |
| G | 740 | SOFA TEST STORES | 188, 205, 215, 257, 262, 406, 458, 499, 535, 1838 |
| I | 10 | NC & VA STORES *(seed)* | 406, 458, 1838 |
| I | 20 | GEORGIA STORES *(seed)* | 215, 257 |
| I | 30 | MISSISSIPPI STORES *(seed)* | 205, 262 |
| I | 40 | FL KY MD STORES *(seed)* | 188, 499, 535 |

`S` (store) takes any store or warehouse number from the tables above.

## Seasons

| Code | Season |
|---|---|
| 0 | NON SEASONAL ITEM |
| 6 | SPRING/SUMMER 2026 |
| 7 | FALL/WINTER 2026 |

## Departments

*Ticketed* = SKUs TS0110 keeps when *Ticketed skus only* = Y.

| Dept | Name | SKUs | of which seeded | Ticketed |
|---|---|---|---|---|
| 12 | LADIES INTIMATE BASICS | 145 | 0 | 90 |
| 59 | BEDDING | 13 | 12 | 12 |
| 86 | SEASONAL CANDY | 13 | 12 | 10 |
| 1 | CANDY/SNACKS | 1 | 0 | 0 |
| 2 | HEALTH & BEAUTY AIDS | 1 | 0 | 0 |
| 7 | KIDS BASICS | 1 | 0 | 1 |
| 17 | KITCHEN & BATH DOMESTICS | 1 | 0 | 0 |
| 22 | MENS/BM BASIC TOPS | 1 | 0 | 1 |
| 24 | HOUSEHOLD CHEMICALS/PAPER | 1 | 0 | 0 |
| 28 | FOOD & DRINKS | 1 | 0 | 0 |
| 32 | LADIES SWIMWEAR | 1 | 0 | 0 |
| 40 | MISSES TOPS | 1 | 0 | 1 |
| 56 | FAMILY ACCESSORIES | 1 | 0 | 1 |
| 74 | DIRECT SERVICE VENDORS | 1 | 0 | 0 |
| 75 | FOOD & DRINK | 1 | 0 | 0 |
| 78 | HOUSEHOLD CHEMICALS/PAPER | 1 | 0 | 0 |
| 162 | DSD FURNITURE | 1 | 0 | 1 |
| 165 | DOVE | 1 | 0 | 0 |

## SKU catalog

Columns: **Sub/Cls** sub-department and class · **Pack** store ship pack · **Retail** chain price · **Tkt** ticketed (TS0110 filter) · **Map** has a replenishment profile for the stores (TS0110 Map column) · **DC 9001 / 9002** warehouse on hand · **P&H** pack-and-hold units across both DCs · **Alloc** units allocated to transfers not yet shipped · **Stores** sample stores with a balance.

### Dept 1 CANDY/SNACKS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 110600 | SOFT PEPPERMINT PUFFS | 1/118 Piedmont | 0 | Piedmont Candy Co. | 24 | 1.25 |  |  | 16,428 | 24 |  | 2,904 | 10 |

### Dept 2 HEALTH & BEAUTY AIDS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 361653 | DOVE PINK BAR | 4/15 Dove Bar Single | 0 | Global Inventory Solutions Corp | 48 | 1.25 |  |  | 162,624 | 107,568 |  | 6,096 | 10 |

### Dept 7 KIDS BASICS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 841039 | IRR BOYS S/S GILDAN | 1/101 Boysbasictees($1.00)$1.49 | 0 | Variety Sales/Abc Hosiery/Grady | 72 | 1.99 | Y |  | 0 | 31,824 |  | 1,584 | 10 |

### Dept 12 LADIES INTIMATE BASICS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 787780 | 2PK ASST BRA | 12/101 2Pk Bras/Ff Bra | 0 | Panties Plus, Inc. | 6 | 7.99 |  | Y | 0 | 1,014 |  | 708 | 10 |
| 787781 | FF 2PK ASST BRA | 12/101 2Pk Bras/Ff Bra | 0 | Panties Plus, Inc. | 6 | 7.99 |  | Y | 0 | 774 |  | 654 | 10 |
| 827412 | FF FASH UW BRA | 12/101 2Pk Bras/Ff Bra | 0 | Dreamwear, Inc | 12 | 5.99 |  | Y | 696 | 1,884 |  | 66 | 10 |
| 351404 | 2X WHITE CONTROL BRIEF | 20/108 Control Briefs | 0 | Richmont Industries Llc | 3 | 4.00 |  |  | 648 | 288 |  |  | 10 |
| 351407 | flex4X WHITE CONTROL BRIEF | 20/108 Control Briefs | 0 | Richmont Industries Llc | 3 | 4.00 |  |  | 0 | 0 |  |  | 10 |
| 827408 | 3PK MCR SHAP BRF | 20/108 Control Briefs | 0 | Dreamwear, Inc | 6 | 9.99 |  |  | 1,638 | 2,484 |  |  | 10 |
| 827414 | 2PK LNG LEG CONTROL | 20/108 Control Briefs | 0 | Dreamwear, Inc | 6 | 9.99 |  |  | 720 | 864 |  |  | 10 |
| 787782 | BRA PANTY SET | 25/102 Ms Bra/Bikini Sets | 0 | Panties Plus, Inc. | 6 | 7.99 |  | Y | 0 | 0 |  | 6 | 10 |
| 787783 | FF BRA PANTY SET | 25/104 Ff Bra/Bikini Sets | 0 | Panties Plus, Inc. | 6 | 7.99 |  | Y | 0 | 342 |  | 30 | 10 |
| 520792 | 2PK MISSY SEAMLESS BRIEF | 27/103 2Pk Seamless Lingerie | 0 | Richmont Industries Llc | 6 | 4.99 | Y |  | 810 | 792 |  |  | 10 |
| 413449 | WUNDERWEAR 5PK BRIEF | 27/104 Multi Pack Panty | 0 | Sutton & Company Apparel Llc | 48 | 4.99 | Y | Y | 528 | 3,504 |  | 1,632 | 10 |
| 413450 | WUNDERWEAR 5PK HI CUT | 27/104 Multi Pack Panty | 0 | Sutton & Company Apparel Llc | 48 | 4.99 | Y | Y | 384 | 2,688 |  | 1,392 | 10 |
| 413451 | WUNDERWEAR 5PK BIKINI | 27/104 Multi Pack Panty | 0 | Sutton & Company Apparel Llc | 48 | 4.99 | Y | Y | 192 | 1,440 |  | 384 | 10 |
| 385697 | 36 C WHT FF CTN BRA | 32/121 #6716 Blkwht Bestform Flx | 0 | Vf Intimates, Lp | 1 | 7.50 | Y |  | 0 | 0 |  |  | 0 |
| 385731 | 38 C WHT CTN WIRE FREE BRA | 32/123 #6233 Blkwht Bestform Vmi | 0 | Vf Intimates, Lp | 1 | 7.50 | Y |  | 0 | 0 |  |  | 0 |
| 385747 | 42 DD BLK CTN WIRE FREE BRA | 32/123 #6233 Blkwht Bestform Vmi | 0 | Vf Intimates, Lp | 1 | 7.50 | Y |  | 0 | 0 |  |  | 0 |
| 424966 | flexPLUS 2PK CTN PAD SPORT BRA | 33/102 Sports Bras | 0 | Richmont Industries Llc | 6 | 7.99 | Y | Y | 0 | 0 |  |  | 10 |
| 426775 | 2PK ASST COTTON MESH SPORT BRA | 33/102 Sports Bras | 0 | Panties Plus, Inc. | 12 | 7.99 | Y | Y | 240 | 912 |  | 348 | 10 |
| 824115 | 38C WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 3 |  | 12 | 10 |
| 824116 | 40C WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 438 |  | 42 | 10 |
| 824118 | 42C WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 824120 | 44C WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 321 |  | 45 | 10 |
| 824122 | 38D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 15 | 288 |  | 54 | 10 |
| 824124 | 40D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 3 |  | 18 | 10 |
| 824126 | 42D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 30 | 510 |  | 54 | 10 |
| 824130 | 44D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 15 |  | 36 | 10 |
| 824132 | 46D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 3 |  | 12 | 10 |
| 824134 | 48D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 6 |  | 24 | 10 |
| 824137 | 42DD WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 363 |  | 48 | 10 |
| 824139 | 44DD WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 6 |  | 24 | 10 |
| 824142 | 38C WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 84 | 1,029 |  | 39 | 10 |
| 824145 | 40C WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 18 | 306 |  | 27 | 10 |
| 824147 | 42C WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 732 |  | 9 | 10 |
| 824148 | 44C WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 519 |  | 15 | 10 |
| 824150 | 38D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 453 |  | 15 | 10 |
| 824151 | 40D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 84 | 0 |  | 18 | 10 |
| 824153 | 42D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 114 | 450 |  | 21 | 10 |
| 824154 | 44D WHITE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 189 |  | 18 | 10 |
| 824155 | 38C NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 6 | 0 |  | 18 | 10 |
| 824156 | 40C NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 162 |  | 6 | 10 |
| 824157 | 42C NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 423 |  | 3 | 10 |
| 824158 | 44C NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 81 | 219 |  | 63 | 10 |
| 824159 | 38D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 9 | 12 |  | 15 | 10 |
| 824160 | 40D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 555 |  | 6 | 10 |
| 824163 | 42D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 39 |  | 69 | 10 |
| 824165 | 44D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 576 |  | 12 | 10 |
| 824182 | 44D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y |  | 0 | 0 |  |  | 0 |
| 824183 | 44D BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 129 |  | 207 | 10 |
| 824184 | 42D BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 63 |  | 90 | 10 |
| 824185 | 38D BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 18 | 0 |  | 12 | 10 |
| 824186 | 38C BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 6 |  | 24 | 10 |
| 824188 | 44DD BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  | 6 | 10 |
| 824190 | 42DD BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 824192 | 48D BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 18 |  | 183 | 10 |
| 824195 | 46D BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 78 | 0 |  | 18 | 10 |
| 824198 | 40C BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 9 | 0 |  | 21 | 10 |
| 824199 | 42C BLK SATIN WIREDREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 126 | 0 |  | 45 | 10 |
| 824204 | 44C BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 824205 | 40D BLK SATIN WIREFREE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 126 |  | 63 | 10 |
| 824206 | 38C BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 824207 | 40C BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 249 |  | 18 | 10 |
| 824208 | 42C BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 824209 | 44C BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 9 | 3 |  | 33 | 10 |
| 824210 | 38D BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 48 | 0 |  | 21 | 10 |
| 824211 | 40D BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 824212 | 42D BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 63 |  | 45 | 10 |
| 824213 | 44D BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 33 |  | 150 | 10 |
| 824214 | 46D BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 24 | 0 |  | 18 | 10 |
| 824215 | 48D BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 87 |  | 183 | 10 |
| 824216 | 42DD BLACK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 824218 | 44DD BLK COTTON BRA W/LACE | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  | 21 | 10 |
| 824220 | 38C NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 824222 | 46D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 21 | 99 |  | 36 | 10 |
| 824235 | 40C NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 75 | 231 |  | 21 | 10 |
| 824236 | 42C NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 27 |  | 15 | 10 |
| 824237 | 44C NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 63 |  | 12 | 10 |
| 824242 | 38D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 12 | 105 |  | 42 | 10 |
| 824243 | 40D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 45 |  | 9 | 10 |
| 824246 | 42D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 39 | 129 |  | 12 | 10 |
| 824251 | 44D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 171 |  | 9 | 10 |
| 824253 | 48D NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 48 |  | 21 | 10 |
| 824255 | 42DD NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 321 |  | 15 | 10 |
| 824258 | 44DD NUDE BRA | 34/105 5.99 Bras | 0 | Dolce Vita Intimates/Lady Ester | 3 | 5.99 | Y | Y | 0 | 0 |  |  | 10 |
| 294044 | S WHT FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 192 |  |  | 6 |
| 294052 | M WHT FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 0 |  |  | 10 |
| 294053 | L WHT FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 0 |  | 20 | 10 |
| 294054 | XL WHT FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 0 |  | 4 | 6 |
| 294055 | S BLK FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 0 |  |  | 6 |
| 294056 | M BLK FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 0 |  |  | 10 |
| 294058 | L BLK FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 0 |  | 4 | 10 |
| 294059 | XL BLK FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 4 |  | 4 | 6 |
| 294060 | 1X WHT  FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 208 |  |  | 10 |
| 294061 | 2X WHT FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 143 | 136 |  | 16 | 10 |
| 294062 | 3X WHT FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 152 |  | 28 | 10 |
| 294063 | 1X BLK FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 216 | 280 |  | 36 | 10 |
| 294064 | 2X BLK FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 216 | 200 |  | 24 | 10 |
| 294065 | 3X BLK FULL SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 4 | 7.50 |  | Y | 0 | 32 |  | 48 | 10 |
| 294066 | M WHT 24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 228 | 404 |  | 6 | 10 |
| 294067 | L WHT  24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 188 | 220 |  | 36 | 10 |
| 294068 | M BLK 24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 192 | 169 |  |  | 10 |
| 294069 | L BLK  24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 240 | 246 |  | 18 | 10 |
| 294070 | 1X WHT 24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 200 | 173 |  |  | 10 |
| 294072 | 2X WHT 24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 188 | 371 |  | 144 | 10 |
| 294074 | 3X WHT 24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 180 | 148 |  | 12 | 10 |
| 294075 | 1X BLK 24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 190 | 438 |  |  | 10 |
| 294080 | 2X BLK 24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 190 | 290 |  | 6 | 10 |
| 294082 | 3X BLK 24 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.99 |  | Y | 194 | 278 |  | 12 | 10 |
| 294083 | M WHT 20 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.50 |  | Y | 432 | 10 |  |  | 10 |
| 294084 | L WHT 20 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.50 |  | Y | 134 | 132 |  | 18 | 10 |
| 294085 | M BLK 20 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.50 |  | Y | 186 | 200 |  |  | 10 |
| 294086 | L BLK 20 INCH HALF SLIP | 36/103 Half/Full Slips | 0 | Panties Plus, Inc. | 6 | 4.50 |  | Y | 378 | 224 |  | 186 | 10 |
| 493935 | BLACK MISSY T SHIRT BRA | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 8 | 5.00 | Y |  | 0 | 0 |  |  | 0 |
| 493970 | FASHION MISSY CONVERTIBLE BRA | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 8 | 5.00 | Y |  | 0 | 0 |  |  | 0 |
| 494315 | ASSORTED MISSY T SHIRT BRAS | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 1 | 360.00 | Y |  | 0 | 0 |  |  | 0 |
| 494316 | ASSORTED MISSY CONVERTIBLE BRA | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 1 | 360.00 | Y |  | 0 | 0 |  |  | 0 |
| 494317 | ASST MISSY PUSHUP BALCONETTE | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 1 | 360.00 | Y |  | 0 | 0 |  |  | 0 |
| 494318 | ASST BACK SMOOTHING BRA | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 1 | 360.00 | Y |  | 0 | 0 |  |  | 0 |
| 494319 | ASST MISSY T SHIRT BRAS #2 | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 1 | 240.00 | Y |  | 0 | 0 |  |  | 0 |
| 494320 | ASST MISSY CONVERTIBLE BRAS #2 | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 1 | 240.00 | Y |  | 0 | 0 |  |  | 0 |
| 494321 | ASST MISSY PUSH UP BALCONETTE | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 1 | 240.00 | Y |  | 0 | 0 |  |  | 0 |
| 494322 | ASST BACK SMOOTHING BRA #2 | 38/101 Jl Bras | 0 | Concepts Nyc Inc | 1 | 240.00 | Y |  | 0 | 0 |  |  | 0 |
| 563910 | SZ7 4PK COLOR BIKINI | 38/102 Jl Pkg.Panties | 0 | Richmont Industries Llc | 4 | 4.99 | Y |  | 1,248 | 768 |  |  | 10 |
| 8558 | LADIES SINGLE PANTY ASSTD | 40/103 $1.25 Panties | 0 | Van Dale Industries Inc | 180 | 1.25 | Y | Y | 2,838 | 18 |  | 1,254 | 10 |
| 31703 | LADIES ASSORTED PANTY | 40/103 $1.25 Panties | 0 | Concepts Nyc Inc | 6 | 1.25 | Y |  | 0 | 0 |  |  | 10 |
| 843016 | SINGLE PANTY | 40/103 $1.25 Panties | 0 | Panties Plus, Inc. | 30 | 1.25 | Y | Y | 0 | 6,750 |  | 3,030 | 10 |
| 843909 | SINGLE PANTY | 40/103 $1.25 Panties | 0 | Dolce Vita Intimates/Lady Ester | 60 | 1.25 |  | Y | 0 | 13,440 |  | 720 | 10 |
| 849528 | LADIES ASSORTED PANTIES | 40/103 $1.25 Panties | 0 | Julius Young,Inc. | 60 | 1.25 | Y | Y | 3,780 | 6,240 |  | 2,520 | 10 |
| 849623 | 2PK PAD UW | 51/1 Girls Bras | 0 | Allura Imports Inc | 12 | 6.99 |  | Y | 960 | 24 |  | 576 | 10 |
| 849638 | BB 2PK MOLDED BRA | 51/1 Girls Bras | 0 | Panties Plus, Inc. | 6 | 6.99 |  | Y | 222 | 504 |  | 264 | 10 |
| 789425 | 4PK SMLS BRA | 51/2 Girls Seamless Bras | 0 | Panties Plus, Inc. | 6 | 6.99 |  | Y | 54 | 6 |  | 108 | 10 |
| 789514 | 5PK BRALETTE | 51/2 Girls Seamless Bras | 0 | Allura Imports Inc | 12 | 5.99 |  | Y | 0 | 12 |  | 48 | 10 |
| 849533 | 4PK SEAMLESS BRA | 51/2 Girls Seamless Bras | 0 | Allura Imports Inc | 6 | 6.99 |  | Y | 864 | 6 |  | 384 | 10 |
| 849535 | 2PK SEAMLESS COOKIE BRA | 51/2 Girls Seamless Bras | 0 | Allura Imports Inc | 6 | 5.99 |  | Y | 864 | 0 |  | 516 | 10 |
| 849537 | 2PK SEAMLESS LACE | 51/2 Girls Seamless Bras | 0 | Allura Imports Inc | 6 | 5.99 |  | Y | 156 | 48 |  | 186 | 10 |
| 849539 | 2PK SEAMLESS CAMI | 51/2 Girls Seamless Bras | 0 | Allura Imports Inc | 6 | 6.99 |  | Y | 156 | 12 |  | 120 | 10 |
| 849649 | SP 2PK SMLS BRA/PADS | 51/2 Girls Seamless Bras | 0 | Panties Plus, Inc. | 6 | 5.99 |  | Y | 354 | 306 |  | 108 | 10 |
| 849531 | SMLS SPORT BRA | 51/3 Girls Sports Bras | 0 | Allura Imports Inc | 6 | 5.99 |  | Y | 528 | 12 |  | 708 | 10 |
| 42322 | BB 10 PK PANTY | 52/2 Girls Pkg Underwear | 0 | Panties Plus, Inc. | 6 | 7.99 |  | Y | 258 | 156 |  | 180 | 10 |
| 787784 | 10PK GIRLS UW | 52/2 Girls Pkg Underwear | 0 | Panties Plus, Inc. | 6 | 6.99 |  | Y | 0 | 857 |  | 288 | 10 |
| 789494 | 7PK 7 14 PANTY | 52/2 Girls Pkg Underwear | 0 | Allura Imports Inc | 6 | 4.99 |  | Y | 498 | 6 |  | 210 | 10 |
| 789497 | 7PK 4 6X PANTY | 52/2 Girls Pkg Underwear | 0 | Allura Imports Inc | 6 | 4.99 |  | Y | 894 | 18 |  | 216 | 10 |
| 789498 | 7PK TODD PANTY | 52/2 Girls Pkg Underwear | 0 | Allura Imports Inc | 6 | 4.99 |  | Y | 258 | 12 |  | 144 | 10 |
| 849645 | BB 5PK BLK | 52/2 Girls Pkg Underwear | 0 | Panties Plus, Inc. | 6 | 6.99 |  | Y | 354 | 12 |  | 114 | 10 |
| 849650 | SP 4PK SMLS PANTY | 52/2 Girls Pkg Underwear | 0 | Panties Plus, Inc. | 6 | 6.99 |  | Y | 0 | 294 |  | 72 | 10 |
| 12 | DEPARTMENT PRICE SKU | 90/1 Discontinued | 0 | 9999 - Miscellaneous | 1 | 0.00 | Y |  | 0 | 0 |  |  | 10 |

### Dept 17 KITCHEN & BATH DOMESTICS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 100001 | 13x13 WASH CLOTH | 5/501 .50 Washcloth | 0 | S M Products,Inc. | 288 | 0.50 |  |  | 895,104 | 1,075,536 |  | 115,776 | 10 |

### Dept 22 MENS/BM BASIC TOPS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 820093 | FOL POCKET TEES | 3/300 $2.49 Basic Ss Tees | 0 | Julius Young,Inc. | 72 | 2.49 | Y |  | 648 | 1,584 |  | 2,160 | 10 |

### Dept 24 HOUSEHOLD CHEMICALS/PAPER

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 159258 | STEAM PAN 1/2 SIZE | 12/6 Aluminum Steam Pans | 0 | Durable Inc | 100 | 1.00 |  |  | 271,867 | 410,778 |  | 76,100 | 10 |

### Dept 28 FOOD & DRINKS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 729934 | ALKA 65 ALKALINE WATER 1.5L | 7/704 Water | 0 | Unix Packaging, Llc | 12 | 1.00 |  |  | 20,700 | 11,232 |  | 3,192 | 10 |

### Dept 32 LADIES SWIMWEAR

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 166402 | ASST SWIM SEPARATE TOPS VEN | 3/4 (Sp) Ms Swimsuit Separate | 6 | Merch-American Exchange Time Llc | 12 | 4.99 |  |  | 3,732 | 612 | 6,432 |  | 10 |

### Dept 40 MISSES TOPS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 262657 | ASST TOPS | 88/7 Missy Rounder Knit | 6 | Fashion Apparel Industries,Inc. | 30 | 5.99 | Y |  | 92,250 | 91,552 | 7,920 |  | 10 |

### Dept 56 FAMILY ACCESSORIES

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 289999 | ASST BASIC BEANIE | 13/106 Fw Mens Basic Beanies | 7 | Luciana Headwear | 24 | 1.99 | Y |  | 0 | 0 |  |  | 10 |

### Dept 59 BEDDING

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 104027 | 2PK PILLOWCASE | 1/101 Microfiber Pillowcases | 0 | Slny Home Inc | 48 | 1.00 | Y |  | 0 | 0 |  |  | 10 |
| 880201 *seed* | 2PK MICROFIBER PILLOWCASE STD | 1/101 Microfiber Pillowcases | 0 | Slny Home Inc | 12 | 3.00 | Y | Y | 3,402 | 5,616 |  |  | 10 |
| 880202 *seed* | MICROFIBER SHEET SET TWIN | 1/102 Sheet Sets | 0 | Home Expressions Textiles | 6 | 9.00 | Y | Y | 945 | 1,560 |  |  | 10 |
| 880203 *seed* | MICROFIBER SHEET SET FULL | 1/102 Sheet Sets | 0 | Home Expressions Textiles | 6 | 10.00 | Y | Y | 756 | 1,248 |  |  | 10 |
| 880204 *seed* | MICROFIBER SHEET SET QUEEN | 1/102 Sheet Sets | 0 | Home Expressions Textiles | 6 | 12.00 | Y | Y | 1,134 | 1,872 |  |  | 10 |
| 880205 *seed* | FLANNEL SHEET SET QUEEN | 1/102 Sheet Sets | 7 | Pem America Inc | 4 | 18.00 | Y | Y | 567 | 936 | 960 |  | 10 |
| 880206 *seed* | 7PC COMFORTER SET QUEEN | 2/201 Comforter Sets | 0 | Pem America Inc | 2 | 30.00 | Y | Y | 227 | 374 |  |  | 10 |
| 880207 *seed* | 5PC COMFORTER SET TWIN | 2/201 Comforter Sets | 0 | Pem America Inc | 2 | 25.00 | Y |  | 227 | 374 |  |  | 10 |
| 880208 *seed* | REVERSIBLE COMFORTER FULL/QN | 2/201 Comforter Sets | 0 | Slny Home Inc | 4 | 18.00 |  | Y | 378 | 624 |  |  | 10 |
| 880209 *seed* | SHERPA THROW 50X60 | 3/301 Throws | 7 | The Northwest Company Llc | 6 | 10.00 | Y | Y | 3,024 | 4,992 | 1,440 |  | 10 |
| 880210 *seed* | PLUSH FLEECE THROW 50X60 | 3/301 Throws | 7 | The Northwest Company Llc | 6 | 5.00 | Y | Y | 4,158 | 6,864 |  |  | 10 |
| 880211 *seed* | HOLIDAY PRINT THROW 50X60 | 3/301 Throws | 7 | The Northwest Company Llc | 6 | 7.00 | Y | Y | 2,646 | 4,368 | 1,800 |  | 10 |
| 880212 *seed* | PLUSH BLANKET QUEEN | 3/302 Blankets | 7 | Slny Home Inc | 4 | 15.00 | Y |  | 378 | 624 |  |  | 10 |

### Dept 74 DIRECT SERVICE VENDORS

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 418842 | LAYS REGULAR 1OZ | 6/100 Single Serve | 0 | Frito-Lay Inc | 1 | 0.66 |  |  | 0 | 0 |  |  | 10 |

### Dept 75 FOOD & DRINK

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 438888 | 24PK .5 LITER WATER | 7/704 Water | 0 | Cg Roxane (Crystal Geyser) Llc | 84 | 3.69 |  |  | 5,796 | 3,108 |  | 8,568 | 10 |

### Dept 78 HOUSEHOLD CHEMICALS/PAPER

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 701735 | AWESOME BLEACH LAV 96OZ | 3/303 Bleach | 0 | Derm Cosmetics/ Awesome Products | 6 | 1.50 |  |  | 5,040 | 5,052 |  | 10,362 | 10 |

### Dept 86 SEASONAL CANDY

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 880110 *seed* | CANDY CORN 11OZ | 3/30 Fall Candy | 7 | Ferrara Candy Company | 12 | 2.00 | Y | Y | 4,536 | 7,488 |  |  | 10 |
| 880111 *seed* | AUTUMN MIX CANDY 11OZ | 3/30 Fall Candy | 7 | Ferrara Candy Company | 12 | 2.00 | Y | Y | 3,402 | 5,616 |  |  | 10 |
| 880112 *seed* | CARAMEL APPLE POPS 12CT | 3/30 Fall Candy | 7 | Piedmont Candy Co. | 12 | 2.25 | Y |  | 1,890 | 3,120 |  |  | 10 |
| 503589 | LDM HOLIDAY COOKIES 9OZ | 4/19 Cookies | 0 | Abimar Foods Inc | 12 | 1.39 |  |  | 0 | 0 |  |  | 10 |
| 880101 *seed* | HOLIDAY SUGAR COOKIES 10OZ | 4/19 Cookies | 7 | Abimar Foods Inc | 12 | 2.00 | Y | Y | 5,292 | 8,736 |  |  | 10 |
| 880102 *seed* | GINGERBREAD COOKIE TIN 12OZ | 4/19 Cookies | 7 | Abimar Foods Inc | 12 | 4.00 | Y | Y | 2,646 | 4,368 |  |  | 10 |
| 880103 *seed* | PEPPERMINT CANDY CANES 12CT | 4/20 Candy Canes | 7 | Spangler Candy Company | 24 | 1.25 | Y | Y | 8,316 | 13,728 | 7,200 |  | 10 |
| 880104 *seed* | MINI CANDY CANES 24CT | 4/20 Candy Canes | 7 | Spangler Candy Company | 24 | 1.50 | Y | Y | 6,048 | 9,984 |  |  | 10 |
| 880105 *seed* | FRUIT FLAVOR CANDY CANES 12CT | 4/20 Candy Canes | 7 | Spangler Candy Company | 24 | 1.25 |  |  | 4,158 | 6,864 |  |  | 10 |
| 880106 *seed* | MILK CHOCOLATE SANTA 3OZ | 4/21 Chocolate Gifts | 7 | Ferrara Candy Company | 18 | 1.50 | Y | Y | 4,914 | 8,112 | 4,320 |  | 10 |
| 880107 *seed* | ASST CHOCOLATE GIFT BOX 7OZ | 4/21 Chocolate Gifts | 7 | Ferrara Candy Company | 12 | 5.00 | Y | Y | 2,268 | 3,744 | 2,880 |  | 10 |
| 880108 *seed* | PEPPERMINT BARK 8OZ | 4/21 Chocolate Gifts | 7 | Piedmont Candy Co. | 12 | 3.00 | Y | Y | 3,402 | 5,616 |  |  | 10 |
| 880109 *seed* | CHOC COVERED PRETZELS 8OZ | 4/21 Chocolate Gifts | 7 | Piedmont Candy Co. | 12 | 2.50 |  |  | 3,024 | 4,992 |  |  | 10 |

### Dept 162 DSD FURNITURE

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 248117 | 1OF2 4176 MELON NAVY LAF SOFA | 8/100 Stationary Sectional | 0 | Delta Furniture Mfg Llc | 1 | 299.99 | Y |  | 0 | 0 |  |  | 3 |

### Dept 165 DOVE

| SKU | Description | Sub/Cls | Season | Vendor | Pack | Retail | Tkt | Map | DC 9001 | DC 9002 | P&H | Alloc | Stores |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 556614 | DOVE SHEA BUTTER BAR | 2/1 $1.25 135G Single Bar | 0 | Akr Corp | 48 | 1.25 |  |  | 0 | 1,176 |  | 2,592 | 10 |

## Vendors

| Vendor | Name | SKUs |
|---|---|---|
| 242 | AKR CORP | 1 |
| 1430 | ALLURA IMPORTS INC | 10 |
| 4278 | DERM COSMETICS/ AWESOME PRODUCTS | 1 |
| 6874 | VF INTIMATES, LP | 3 |
| 9999 | 9999 - MISCELLANEOUS | 1 |
| 10032 | CG ROXANE (CRYSTAL GEYSER) LLC | 1 |
| 13660 | CONCEPTS NYC INC | 11 |
| 16332 | DELTA FURNITURE MFG LLC | 1 |
| 17486 | DREAMWEAR, INC | 3 |
| 17974 | DURABLE INC | 1 |
| 20826 | FASHION APPAREL INDUSTRIES,INC. | 1 |
| 22850 | FERRARA CANDY COMPANY | 4 |
| 23177 | FRITO-LAY INC | 1 |
| 23296 | ABIMAR FOODS INC | 3 |
| 25513 | GLOBAL INVENTORY SOLUTIONS CORP | 1 |
| 31405 | HOME EXPRESSIONS TEXTILES | 3 |
| 38640 | DOLCE VITA INTIMATES/LADY ESTER | 66 |
| 41974 | LUCIANA HEADWEAR | 1 |
| 54778 | PANTIES PLUS, INC. | 41 |
| 57353 | PIEDMONT CANDY CO. | 4 |
| 59780 | THE NORTHWEST COMPANY LLC | 3 |
| 60215 | PEM AMERICA INC | 3 |
| 62896 | RICHMONT INDUSTRIES LLC | 5 |
| 65897 | S M PRODUCTS,INC. | 1 |
| 68120 | SPANGLER CANDY COMPANY | 3 |
| 71188 | SLNY HOME INC | 4 |
| 74434 | SUTTON & COMPANY APPAREL LLC | 3 |
| 76331 | MERCH-AMERICAN EXCHANGE TIME LLC | 1 |
| 78735 | UNIX PACKAGING, LLC | 1 |
| 79249 | VAN DALE INDUSTRIES INC | 1 |
| 79357 | VARIETY SALES/ABC HOSIERY/GRADY | 1 |
| 84623 | JULIUS YOUNG,INC. | 2 |

## How the data was built

- `prfdemolib/tools/load-prfdemo-data.sh <LIB>` copies the PRFDEMOLIB sample with CPYF, then runs the seed. Add `--no-seed` for the shipped sample only.
- The seed comes from `prfdemolib/tools/gen-seed-data.py`, which documents every item. Each seeded row is a copy of a real sample row (SKU 849528 for balances and history; 503589 and 104027 for the item master). Identity, hierarchy, vendor, pack, cost and price are replaced, and quantities are scaled per item and varied per store, so the screens behave exactly as they do for the real SKUs.
- Seeded SKUs have style number `DEMOSEED`. To remove them, rerun the loader with `--no-seed`. Seeded departments 86 and 59 also contain one original PRFDEMOLIB SKU each (503589, 104027).
- Product choices follow Roses departments listed on rosesdiscountstores.com and vwstores.com (Seasonal, Home, Food & Beverage) and the current Fall/Holiday 2026 season.
