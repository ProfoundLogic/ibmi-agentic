# Retail Inquiry Programs: New Hire Guide

**Who this is for:** anyone using the three retail inquiry programs on the
*Agentic Coding Demo Menu* for the first time. You don't need any IBM i
experience. Section 2 covers the green-screen basics.

**What you'll be able to do by the end:**

- look up how much of an item each store has, and how it is selling (INV046)
- look up an item's details and warehouse stock (INV502)
- review a store department's ticketed items and send it more stock (TS0110)

> **About these programs.** They come from a retail merchandising system (JDA
> MMS) used by Variety Wholesalers, which runs *Roses* stores. This copy is a
> **demo**. It has a small sample of real data (16 locations, 162 items) plus
> 24 holiday candy and bedding items seeded for the demo, and links to none of
> the other programs the full system has. There are no F4 prompts, so keep the
> **[Sample Data Reference](SAMPLE-DATA.md)** open while you work: it lists
> every store, group, department and SKU you can key in. Everything is
> read-only except TS0110's *Create distribution*, which writes to a demo work
> file and creates no real transfers. You can't break anything.

---

## Contents

1. [Before you start](#1-before-you-start)
2. [Green-screen basics in five minutes](#2-green-screen-basics-in-five-minutes)
3. [The menu](#3-the-menu)
4. [Option 4: Inventory by Store (INV046)](#4-option-4-inventory-by-store-inv046)
5. [Option 5: Item Inquiry (INV502)](#5-option-5-item-inquiry-inv502)
6. [Option 6: Ticket Item Inventory (TS0110)](#6-option-6-ticket-item-inventory-ts0110)
7. [Sample data at a glance](#7-sample-data-at-a-glance)
8. [When something goes wrong](#8-when-something-goes-wrong)
9. [Glossary](#9-glossary)
10. [Signing off](#10-signing-off)

---

## 1. Before you start

You need three things:

| You need | Why |
|---|---|
| **A sign-on to the IBM i** (the demo profile, `AIDEMO`, in our environment) through Profound UI Genie in your browser, or another 5250 emulator | This is how you reach the menu |
| **The demo library on your library list** | The programs and their data live in one library (in this environment, the task library, e.g. `AITSK00095`). If menu options 4-6 are missing, see [section 8](#8-when-something-goes-wrong). |
| **A wide (27 x 132) display** | Item Inquiry and Ticket Item Inventory use the wide screen. Genie supports it automatically. On a 3270-style 24 x 80 terminal, TS0110 shows a "wider screen needed" notice and INV502 will not run. |

When you sign on you may first see a **Display Program Messages** screen saying
*"Message queue AIDEMO is allocated to another job."* That's normal: someone
else is signed on with the same demo profile. **Press Enter** to continue.

---

## 2. Green-screen basics in five minutes

These programs use a *5250* (green-screen) interface. A few habits make it easy.

| To... | Do this |
|---|---|
| Move between entry fields | **Tab** (forward) and **Shift+Tab** (back). The mouse also works in Genie. |
| Submit what you typed | **Enter**. Nothing happens until you press Enter or a function key. |
| Use a command shown at the bottom (e.g. `F5=PTD/YTD`) | Press that function key: **F5**. In these programs **F1 means Exit or Return**, not Help. |
| See more rows of a list | **Page Down** / **Page Up** (sometimes labelled *Roll* keys) |
| Clear a field | Type over it with spaces, or use **Field Exit** / **End** to blank the rest of the field |
| Get rid of an error and unlock the keyboard | Press your emulator's **Reset** key, then correct the field |

Tips:

- **Numbers are right-aligned.** Type `205` in a store field and it appears as
  `  205`. You don't need leading zeros.
- **Error messages appear on the bottom line** of the screen, or in a small
  pop-up window.
- The **screen name** is in the top corner (for example `INV046`, `INV502 2`,
  `TS0110 SKUSFL`). Mention it when you ask someone for help.

---

## 3. The menu

After sign-on you land on the **Agentic Coding Demo Menu**. Options 4-12 are the
retail programs: three programs, each shown three ways.

| Options | What you get |
|---|---|
| 4, 5, 6 | The original green screens (this guide's sections 4-6) |
| 7, 8, 9 | The same programs as plain web pages: the green screen, field for field, in a browser ([section 8a](#8a-the-web-versions)) |
| 10, 11, 12 | The same programs again in the Variety Wholesalers web experience, with search pickers, sorting, export and more ([section 8a](#8a-the-web-versions)) |

```text
 MENU                       Agentic Coding Demo Menu

 Select one of the following:

      1. Work with Customers             4. Inventory by Store (INV046)
      2. Work with Customers (RPGOA)     5. Item Inquiry (INV502)
      3. Work with Customers (EJS)       6. Ticket Item Inventory (TS0110)
                                         7. Inventory by Store (Web)
                                         8. Item Inquiry (Web)
     90. Sign off                        9. Ticket Item Inventory (Web)
                                        10. Inventory by Store (VW Web)
                                        11. Item Inquiry (VW Web)
                                        12. Ticket Item Inventory (VW Web)







 Selection:
 ===>
```

1. Type the option number (`4` to `12`) next to **Selection**.
2. Press **Enter**.
3. When you exit a program you come back here. **90** signs you off.

The `===>` line is a command line. You don't need it for this guide.

---

## 4. Option 4: Inventory by Store (INV046)

**What it answers:** *"For this item, how much does each store have, how much
has it sold, and how many weeks of supply is left?"* You can look at a region,
a district, a store group, or start at one store.

### 4.1 The entry screen

```text
    INV046                Variety Wholesalers, Inc.             Date: 11/03/26
                      Current Period Inventory History          Time: 10:42:41
  SKU Number....:
  Store Type....:  A  (A=All, S=Stores, O=Offices, W=Warehouses, I=Inv Loc)
  Search For....:  S  (R=Region, D=District, S=Store, G=Str Group, I=Inv Group)
  Store Number..:                                   Current Period
    Store   P   On      On                         Net    Tot    8 Week  Wks
 I  Name    S  Hand     Order   Sales    Rec'd    Tfr's   Adj's  Avg.Sls Supply











 Chain

 ============================ JDA Software (4R4) ==============================
 F1=Return  F3=Sku Search  F5=PTD/YTD  F6=Str Group Inquiry  F8=Chain Inquiry
 'I' to Inquire     F4=Search Store/Whs    F7=Cost and Price  Roll Keys to Page
```

| Field | What to enter |
|---|---|
| **SKU Number** | The item number (SKU), e.g. `262657` |
| **Store Type** | Which kinds of locations to list: `A` all, `S` stores only, `O` offices, `W` warehouses, `I` inventory locations |
| **Search For** | How to choose the locations: `R` region, `D` district, `S` store, `G` store group, `I` inventory group |
| **Store Number** (the label changes to match *Search For*) | The region, district, store, store group or inventory group number (all listed in the [Sample Data Reference](SAMPLE-DATA.md#regions-districts-and-groups)) |

### 4.2 Walk-through: one item across a store group

1. Choose option **4** from the menu.
2. Type **262657** in *SKU Number*.
3. Leave *Store Type* as **A**. Change *Search For* to **G**.
4. Type **740** in *Store Group* (the 10-store test group) and press **Enter**.

```text
    INV046                Variety Wholesalers, Inc.             Date: 11/03/26
                      Current Period Inventory History          Time: 10:42:44
  SKU Number....:     262657  ASST TOPS
  Store Type....:  A  (A=All, S=Stores, O=Offices, W=Warehouses, I=Inv Loc)
  Search For....:  G  (R=Region, D=District, S=Store, G=Str Group, I=Inv Group)
  Store Group...:    740  SOFA TEST STORES          Current Period
    Store   P   On      On                         Net    Tot    8 Week  Wks
 I  Name    S  Hand     Order   Sales    Rec'd    Tfr's   Adj's  Avg.Sls Supply
   188 ELIZ Y      73        0       42        0       90      0      5.0    15
   205 LAUR Y      33        0       18        0        0      0      4.0     8
   215 MACO Y      99        0        1        0        0      0      1.1    90
   257 GRIF Y      36        0       13        0        0      0      6.6     5
   262 JACK Y      31        0       49        0        0      0     11.6     3
   406 SOUT Y      31        0        1        0        0      0       .9    34
   458 GRNS Y      17        0        5        0        0      0      1.8     9
   499JCKSN Y      37        0        3        0        2      0       .5    74
   535GLEN  Y      15        0        9        0        0      0      2.6     6
              -------- -------- -------- -------- -------- ------ -------- -----
 Store Group      407        0      143        0       92      0     35.0    12
 Chain         192202       26     1722        0        8    380    581.4   331

 ============================ JDA Software (4R4) ==============================
 F1=Return  F3=Sku Search  F5=PTD/YTD  F6=Str Group Inquiry  F8=Chain Inquiry
 'I' to Inquire     F4=Search Store Group  F7=Cost and Price  Roll Keys to Page
```

5. The item description (**ASST TOPS**) and group name (**SOFA TEST STORES**)
   confirm your entries. Nine stores fit on the page. Press **Page Down** to
   see the tenth (store 1838).
6. Press **F5** to switch the figures between **Current Period** (period to
   date) and **Year to Date**. The title at the top right changes to match.
7. Press **F1** to return to the menu.

### 4.3 Reading the list

| Column | Meaning |
|---|---|
| **Store / Name** | Store number and the store's short name (e.g. `205 LAUR`) |
| **PS** | `Y` if the store is on the point-of-sale system |
| **On Hand** | Units in the store now |
| **On Order** | Units on purchase orders or transfers not yet received |
| **Sales** | Units sold in the period (or year, after F5) |
| **Rec'd** | Units received |
| **Net Tfr's** | Transfers in minus transfers out |
| **Tot Adj's** | Inventory adjustments. A trailing minus (`24-`) means negative. |
| **8 Week Avg. Sls** | Average units sold per week over the last 8 weeks |
| **Wks Supply** | On hand divided by average weekly sales: how many weeks the stock will last |

The two lines under the list are totals:

- **Store Group / Region / District:** the locations you searched for.
- **Chain:** every location in the company.

> **Note:** With *Search For* = **S**, the list **starts at** the store you
> typed and continues in store-number order. It does not show that store alone.
> With *Store Type* = **A** the list continues into the warehouses (9001 and
> up), so the total includes warehouse stock.

### 4.4 Other searches to try

| Try | Result |
|---|---|
| SKU `262657`, Store Type `S`, Search `R`, Region `3` | Stores 406, 535 and 1838, with a *Region* total |
| SKU `262657`, Search `S`, Store `205` | Starts the list at store 205 |
| SKU `100001`, Search `G`, Group `740` | The same ten stores for a basic washcloth |
| SKU `880103`, Search `I`, Inv Group `30` | Mississippi stores (205, 262), peppermint candy canes |
| SKU `880209`, Search `I`, Inv Group `10` | NC and VA stores (406, 458, 1838), sherpa throws |

> **Tip:** with *Search For* = `G` or `I`, leave *Store Type* at **A**. The
> groups contain only stores, so you get the same list. Choosing `S` or `W`
> there can return an **empty** list, especially on the first inquiry after you
> open the program. That's a bug in the original program: it checks the type
> of the previous store it read instead of the current one.

### 4.5 Keys

| Key | Works in the demo? | What it does |
|---|---|---|
| Enter | Yes | Run the inquiry |
| F1 | Yes | Return to the menu |
| F5 | Yes | Toggle period to date / year to date |
| Page Down / Page Up | Yes | Page through the stores |
| F3 SKU Search, F4 Search Store, F6 Store Group Inquiry, F7 Cost and Price, F8 Chain Inquiry, `I` in the left column (Inquire) | **No** | These would open programs that are not part of the demo. They are switched off, so the key **does nothing**: the screen just redisplays. |

Messages you may see on the bottom line:

- *The SKU Entered is Invalid*: check the item number.
- *Region / District / Store / Store Group is not valid*: that number doesn't
  exist in the sample data. See the [Sample Data Reference](SAMPLE-DATA.md#regions-districts-and-groups).
- *Inv Group is not valid*: use inventory group 10, 20, 30 or 40.

---

## 5. Option 5: Item Inquiry (INV502)

**What it answers:** *"Tell me everything about this item: vendor, department,
prices, season, and how much each warehouse has, has on order, and has
committed."*

### 5.1 Step by step

1. Choose option **5**. The screen asks for a **SKU**.

```text
 INV502     1                                        Variety Wholesalers, Inc.                                      Date: 11/03/26
 AIDEMO                                                     Item Inquiry                                            Time: 10:42:58




                                                           SKU:
















                           ============================ JDA Software (4R4) ==============================
 F1=Exit
```

2. Type **262657** and press **Enter**. The detail screen appears:

```text
 INV502     2                                        Variety Wholesalers, Inc.                                      Date: 11/03/26
 AIDEMO                                                     Item Inquiry                                            Time: 10:43:01

      SKU:    262657  ASST TOPS

 Vendor...:  20826  FASHION APPAREL INDUSTRIES,INC     Set Code.: 0                                      Vndr Cost:        1.580
 Vndr Prt#: CO ASST TOPS                               Spcl Hndl:                                        Avg Cost.:        1.5800
 Dept.....:  40     MISSES TOPS                        Repl Code: 0    NOT A REPLENISHMENT ITEM          Orig Pric:        5.99
 Subdept..:  88     HABAND                             Vel Rank.:                                        Chn Pric.:        5.99
 Class....:   7     MISSY ROUNDER KNIT                 No Dwnld.: N                                      Zn 2 Pric:        5.99
 Status...: A       ACTIVE                             Closeout.: Y    Close Out Item                    Zn 3 Pric:        5.99
                                                       Stor Ordr: N    Not Store Orderable (POS Store    1st Sale.:  5/11/26
 Season...: 6       SPRING/SUMMER 2026                 Hard/Soft: S    Soft Good                         Last Sale: 10/28/26
 Prc Label: 1       Softlines Hang Ticket              Ctn Label: N    No Carton Label Code Applicabl    Case.CuFt:         .000
 Order Bk.: 0       0  0  0  0  0  0                   Import...: D    Domestic                          Case Lbs.:         .000

  Whse  On Hnd      OOS      On Ord   Last Ord   Lst Rcvd   Rpk Ord    Alloc'd    In Trns    Pak&Hld    Next Out  Cas Pak Shp Pak
  9001   92250    0/00/00         0    4/22/26    4/29/26         0          0          0      86040     1/03/27       30      30
  9002   91552    0/00/00         0    5/04/26    5/15/26         0          0          0      90900     1/03/27       30      30
  9151       0    0/00/00         0    0/00/00    0/00/00         0          0          0          0     0/00/00       30      30
  9152       0    0/00/00         0    0/00/00    0/00/00         0          0          0          0     0/00/00       30      30


                           ============================ JDA Software (4R4) ==============================
 F1=Exit F2=BYR  F3=DistHd  F4=OrderBook  F5=Open POs  F7=Sets   F8=UPCs  F9=TrfDtl  F10=ShpPt   F11=Count   F12=RcvHst  F13=VndMst
 F14=OldP&H    F16=WhsInvAdj    F17=NewP&H    F18=SKU Aud    F19=Prc Chgs    F20=InvBal     F21=POGs    F22=ItmAtr    F23=JDA Distr
```

3. To look at another item, **type over the SKU** at the top and press
   **Enter**.
4. **Page Down** moves to the *next* SKU number in the file and **Page Up** to
   the *previous* one. It's a quick way to browse the sample items.
5. Press **F1** to return to the menu.

### 5.2 Reading the top half

| Area | What it tells you |
|---|---|
| **Vendor / Vndr Prt#** | Who supplies the item, and the vendor's own part number |
| **Dept / Subdept / Class** | Where the item sits in the merchandise hierarchy |
| **Status** | `A` = active |
| **Season** | e.g. `6 SPRING/SUMMER 2026`, `0 NON SEASONAL ITEM` |
| **Set Code** | Whether the item is part of a set. `6 Bundle Component` means it is sold as part of a bundle. |
| **Repl Code / Vel Rank** | How the item is replenished, and its sales velocity rank |
| **Closeout, Stor Ordr, Hard/Soft, Import** | Item flags, each with its meaning shown beside it |
| **Vndr Cost / Avg Cost** | Cost from the vendor and average cost |
| **Orig Pric / Chn Pric / Zn 2 / Zn 3 Pric** | Original retail, current chain price, and the prices in price zones 2 and 3 |
| **1st Sale / Last Sale** | First and most recent sale dates |

### 5.3 Reading the warehouse grid

One line per warehouse (distribution center, DC): **9001** Henderson,
**9002** Newnan, and **9151/9152**, two more DCs.

| Column | Meaning |
|---|---|
| **On Hnd** | Units in the warehouse |
| **OOS** | Out-of-stock date recorded for the warehouse (`0/00/00` = none) |
| **On Ord / Last Ord / Lst Rcvd** | Units on order from the vendor, last order date, and last receipt date |
| **Rpk Ord** | Units on repack orders |
| **Alloc'd** | Units already allocated to store transfers but not yet shipped |
| **In Trns** | Units in transit |
| **Pak&Hld** | *Pack and hold:* stock set aside for a future event or season |
| **Next Out** | Date the next pack-and-hold quantity is due out |
| **Cas Pak / Shp Pak** | Units per vendor case and per store shipping pack |

Good items to look at:

| SKU | Why it's interesting |
|---|---|
| `262657` or `166402` | Large pack-and-hold quantities |
| `100001` | Allocated quantities at both main warehouses |
| `248117` | A sofa sold as a *Bundle Component* (Set Code 6) |

### 5.4 Keys

| Key | Works in the demo? | What it does |
|---|---|---|
| Enter | Yes | Show the SKU you typed |
| Page Down / Page Up | Yes | Next / previous SKU |
| F1 | Yes | Return to the menu |
| F3 on the SKU prompt, and F2 to F23 on the detail screen (BYR, DistHd, OrderBook, Open POs, ...) | **No** | These would open inquiry programs that are not in the demo. They are switched off, so the key **does nothing**: the screen just redisplays. |

If you type a SKU that doesn't exist, the bottom line shows **"INV0297: Invalid
sku entered."**:

```text

      SKU:    999999  1OF2 4176 MELON NAVY LAF SOFA
   ...
                           ============================ JDA Software (4R4) ==============================
 F1=Exit F2=BYR  F3=DistHd  F4=OrderBook  F5=Open POs  F7=Sets   F8=UPCs  F9=TrfDtl  F10=ShpPt   F11=Count   F12=RcvHst  F13=VndMst
 F14=OldP&H    F16=WhsInvAdj    F17=NewP&H    F18=SKU Aud    F19=Prc Chgs    F20=InvBal     F21=POGs    F22=ItmAtr    F23=JDA Distr
 INV0297: Invalid sku entered.
```

> **Watch out:** after this message the description from the *previous* SKU
> stays on screen. That's how the program has always behaved. Trust the
> message, not the description, and type a valid SKU.

---

## 6. Option 6: Ticket Item Inventory (TS0110)

**What it answers:** *"For one store and department, how has each item sold
since it was shipped, how much can the warehouse still send, and can I send
more?"* It is also the only one of the three that **does** something:
**F10 = Create distribution** builds a list of items to ship to the store.

### 6.1 The prompt screen

```text
  11/03/26                 Variety Wholesalers, Inc.                 TS0110
  10:43:18          TICKET ITEM INVENTORY FOR STORE BY DEPT          PROMPT


     Store:     0

      Dept: 000

    Season: ALL   (Enter ALL for all seasons)

    Ticketed skus only: Y

    Show skus with whse available qty only: Y

    Include Last Year?: Y   (Y/N, N = this year only)







  F1=exit
```

| Field | What to enter |
|---|---|
| **Store** | Store number, e.g. `205` |
| **Dept** | Department: `12` Ladies Intimate Basics (145 items), `86` Seasonal Candy or `59` Bedding (13 items each). Most other departments have a single item. |
| **Season** | `ALL`, or one season code: `0` non-seasonal, `6` Spring/Summer 2026, `7` Fall/Winter 2026 |
| **Ticketed skus only** | `Y` lists only ticketed items. `N` lists all of them. |
| **Show skus with whse available qty only** | `Y` lists only items the warehouse can still ship. `N` lists all of them. |
| **Include Last Year?** | `Y` adds last year's figures. `N` shows this year only. |

### 6.2 Walk-through: review a department

1. Choose option **6**.
2. Type **205** in *Store* and **12** in *Dept*. Leave *Season* as **ALL**.
3. To see the **whole** department, set *Ticketed skus only* and *Show skus
   with whse available qty only* to **N**. Press **Enter**.

```text
  11/03/26    WH9002          Variety Wholesalers, Inc.                   TS0110
  10:43:22             TICKET ITEM INVENTORY FOR STORE BY DEPT            SKUSFL
  Store:   205 LAUREL                MS  Dept: 012 LADIES INTIMATE BASICS
  Ticketed only = N  Available only = N  Season = ALL  Years = TY & LY
 Pks  SkuNo Description Map  Price  Pack EaShp EaOnh EaSld %Sold  Resv  PksAv   P&H   Rsrv
         12 FASHION BRAS
        101 2PK BRAS/FF BRA
     787780 2PK ASST BRA  Y    7.99    6   144     0   144 100.0     0     79      0      0
     787781 FF 2PK ASST B Y    7.99    6   267     0   267 100.0     0     42      0      0
     827412 FF FASH UW BR Y    5.99   12   149     6   143  96.0     0    154      0      0
        101 Class Totals..:                560     6   554  98.9     0    275      0      0
         12 SubDpt Totals.:                560     6   554  98.9     0    275      0      0
         20 CONTROL BRIEFS
        108 CONTROL BRIEFS
     351404 2X WHITE CONT N    4.00    3    35     0    35 100.0     0     96      0      0
     351407 flex4X WHITE  N    4.00    3    28     0    28 100.0     0      0      0      0
     827408 3PK MCR SHAP  N    9.99    6    82     0    82 100.0     0    414      0      0
     827414 2PK LNG LEG C N    9.99    6    12     0    12 100.0     0    144      0      0
        108 Class Totals..:                157     0   157 100.0     0    654      0      0
         20 SubDpt Totals.:                157     0   157 100.0     0    654      0      0
         25 PROMO BRA/BIKIN
                                                              133 skus listed
   F1=exit  F2=prompt  F7=top  F8=bottom              F10=create distribution
```

4. The header shows the store (**205 LAUREL MS**), its supplying warehouse
   (**WH9002**, top left) and your choices. Items are grouped by subdepartment
   and class. The pink/blue lines are headings and subtotals, and the bottom
   line shows the item count (**133 skus listed**).
5. Use **Page Down / Page Up** to scroll, **F7** to jump to the top and **F8**
   to jump to the bottom.
6. **F2** (or F1) returns to the prompt so you can pick another store or
   department. **F1** on the prompt returns to the menu.

With both filters left at **Y**, the same store and department list **40**
items: just the ticketed ones the warehouse can still supply.

### 6.3 Reading the list

| Column | Meaning |
|---|---|
| **Pks** | **Entry field.** The number of *packs* to send in a distribution (see 6.4) |
| **SkuNo / Description** | Item number and the first part of its description |
| **Map** | `Y` if the item is mapped to a replenishment profile for this store |
| **Price** | Store retail price for the store's price zone |
| **Pack** | Units per shipping pack |
| **EaShp** | Each (units) shipped to the store: what it has now plus what it has sold |
| **EaOnh** | Units on hand in the store |
| **EaSld** | Units sold (shipped minus on hand) |
| **%Sold** | Sell-through: units sold as a percentage of units shipped |
| **Resv** | Units already on transfers to the store |
| **PksAv** | Packs the warehouse can still ship (on hand minus what is committed, divided by pack size) |
| **P&H** | Packs held in the warehouse's pack-and-hold |
| **Rsrv** | Pack-and-hold quantity reserved for new stores |

> **How to read it:** a high **%Sold** with **PksAv** greater than zero means
> the store is selling through and the warehouse can send more. That's when
> you'd create a distribution.

### 6.4 Walk-through: create a distribution (F10)

1. On the list, type the number of **packs** in the **Pks** column next to each
   item you want to send. For example, `2` next to **787780** and `1` next to
   **827412**. One pack = the *Pack* column's number of units.
2. Press **F10**. A confirmation window appears:

```text
  11/03/26    WH9002          Variety Wholesalers, Inc.                   TS0110
  10:43:22             TICKET ITEM INVENTORY FOR STORE BY DEPT            SKUSFL
  Store:   205 LAUREL                MS  Dept: 012 LADIES INTIMATE BASICS
  Ticketed only = N  Available only = N  Season = ALL  Years = TY & LY
 Pks  SkuNo Description Map  Price  Pack EaShp EaOnh EaSld %Sold  Resv  PksAv   P&H   Rsrv
         12 FASHION BRAS
        101 2PK B ............................................
 2   787780 2PK A :                                          : 0     0     79      0      0
     787781 FF 2P :                                          : 0     0     42      0      0
 1   827412 FF FA :   Press ENTER to create a distribution   : 0     0    154      0      0
        101 Class :                                          : 9     0    275      0      0
         12 SubDp :   or F1 to cancel.                       : 9     0    275      0      0
         20 CONTR :                                          :
        108 CONTR :                                          :
     351404 2X WH :..........................................: 0     0     96      0      0
     351407 flex4X WHITE  N    4.00    3    28     0    28 100.0     0      0      0      0
     827408 3PK MCR SHAP  N    9.99    6    82     0    82 100.0     0    414      0      0
     827414 2PK LNG LEG C N    9.99    6    12     0    12 100.0     0    144      0      0
        108 Class Totals..:                157     0   157 100.0     0    654      0      0
         20 SubDpt Totals.:                157     0   157 100.0     0    654      0      0
         25 PROMO BRA/BIKIN
                                                              133 skus listed
   F1=exit  F2=prompt  F7=top  F8=bottom              F10=create distribution
```

3. Press **Enter** to create the distribution, or **F1** to cancel and go back
   to the list.
4. You return to the prompt screen. Behind the scenes:
   - a new work member is created, named **T9000001** (then T9000002, and so on)
   - one record is written for each item, with quantity = packs x pack size
     (2 x 6 = 12 units and 1 x 12 = 12 units in the example)
   - a batch job, **CLTS0111**, is submitted to process it. In the demo this job
     only records the member name and record count in its job log. **No real
     transfers are created.**

If you press F10 without entering any packs, you get this notice. Press
**Enter** to go back to the list:

```text
  11/03/26    WH9002          Variety Wholesalers, Inc.                   TS0110
  10:50:17             TICKET ITEM INVENTORY FOR STORE BY DEPT            SKUSFL
  Store:   205 LAUREL                MS  Dept: 012 LADIES INTIMATE BASICS
  Ticketed only = Y  Available only = Y  Season = ALL  Years = TY & LY
 Pks  SkuNo Description Map  Price  Pack EaShp EaOnh EaSld %Sold  Resv  PksAv   P&H   Rsrv
         27 FASHION BIKINIS
        103 2Pk S ............................................
     520792 2PK M :                  NOTICE                  : 8     0    132      0      0
        103 Class :                                          : 8     0    132      0      0
        104 MULTI :    You pressed F10 to create a distri-   :
     413449 WUNDE :    bution, but none were found. You      : 4     0     43      0      0
     413450 WUNDE :    must enter a qty next to the skus     : 0     0     30      0      0
     413451 WUNDE :    to distribute.   Press ENTER.         : 5     0     24      0      0
        104 Class :                                          : 8     0     97      0      0
         27 SubDp :..........................................: 8     0    229      0      0
         33 SPORT BRAS
        102 SPORTS BRAS
     426775 2PK ASST COTT Y    7.99   12   297     5   292  98.3     0     61      0      0
        102 Class Totals..:                297     5   292  98.3     0     61      0      0
         33 SubDpt Totals.:                297     5   292  98.3     0     61      0      0
         34 LADY ESTER BRAS
                                                               40 skus listed
   F1=exit  F2=prompt  F7=top  F8=bottom              F10=create distribution
```

### 6.5 Keys

| Key | Screen | What it does |
|---|---|---|
| Enter | Prompt | Show the list |
| F1 | Prompt | Return to the menu |
| Page Down / Page Up | List | Scroll |
| F7 / F8 | List | Jump to top / bottom |
| F2 or F1 | List | Back to the prompt |
| F10 | List | Create a distribution from the quantities in *Pks* |

If the store or department doesn't exist you'll see, for example, **"ERROR:
Invalid store number - press RESET"** on the bottom line. Press **Reset**, fix
the number, and press Enter.

---

## 7. Sample data at a glance

The full list of keyable values is in the **[Sample Data Reference](SAMPLE-DATA.md)**:
every store, warehouse, region, district, group, season, department, vendor and
all 186 SKUs, generated from the live tables. The values you will use most:

| What | Values |
|---|---|
| Stores (all Roses) | 188, 205, 215, 257, 262, 406, 458, 499, 535, 1838 |
| Warehouses (DCs) | 9001 Henderson NC, 9002 Newnan GA (also 9006, 9007, 9151, 9152) |
| Regions / districts | Regions 1-5. Districts 11, 14, 20, 25, 34, 38, 39, 47, 51, 52. |
| Store group | 740 (all 10 stores) |
| Inventory groups | 10 NC & VA, 20 Georgia, 30 Mississippi, 40 FL/KY/MD |
| Departments with depth | 12 Ladies Intimate Basics, 86 Seasonal Candy, 59 Bedding |
| Seasons | 0 non-seasonal, 6 Spring/Summer 2026, 7 Fall/Winter 2026 |

**Items worth knowing:**

| SKU | Description | Good for |
|---|---|---|
| 262657 | Asst Tops | INV046 store group 740, INV502 pack and hold |
| 100001 | 13x13 Wash Cloth | INV502 allocated quantities, fast seller |
| 248117 | 1of2 4176 Melon Navy LAF Sofa | INV502 bundle component |
| 880103 | Peppermint Candy Canes 12ct | INV046 inventory group 30, INV502/TS0110 pack and hold |
| 880209 | Sherpa Throw 50x60 | INV502 new vendor, Fall/Winter season, pack and hold |
| 880206 | 7pc Comforter Set Queen | A $30 big-ticket item that sells slowly |
| 787780, 827412 | 2pk Asst Bra, FF Fash UW Bra (dept 12) | TS0110 distribution example |

**TS0110:** use store **205** (warehouse 9002) or **406** (warehouse 9001) with
department **12**, **86** or **59**.

---

## 8. When something goes wrong

### "I pressed a function key and nothing happened"

That's expected. The keys listed as **No** in sections 4.5 and 5.4 would open
other programs that aren't part of this demo, so they are switched off and the
screen simply redisplays.

### "Error calling program ..." or "The call to MAIN ended in error"

You shouldn't see this any more. If you do (for example in an older copy of
the demo), a **Display Program Messages** screen asks for a reply. Type **G**
at *Reply* and press **Enter**. INV046 returns to a fresh entry screen and
INV502 returns to the menu.

### Other problems

| Symptom | What to do |
|---|---|
| Keyboard locked, error on the bottom line | Press **Reset**, correct the field, press Enter |
| Menu shows only options 1-3 | Your session doesn't have the demo library on its library list. Sign off and back on. If that doesn't help, ask whoever set up the demo which library to use. |
| Menu was updated but you still see the old one | Your session started before the change. Sign off and sign back on. |
| TS0110 shows a small window saying a larger screen is needed | Your session is 24 x 80. Use Genie or a 27 x 132 session. |
| *Display Program Messages* right after sign-on ("allocated to another job") | Normal on a shared profile. Press Enter. |
| A number shows as `99999` or `999.9` in TS0110 | The value is too big for the column. It's capped for display. |

---

## 8a. The web versions

### Options 7, 8 and 9: the green screen in a browser

These are the same programs as options 4, 5 and 6, shown as web pages that
follow the green screen exactly: the same labels in the same order, the same
codes to type (`A`/`S`/`O`/`W`/`I`, `R`/`D`/`S`/`G`/`I`, `Y`/`N`), the same
messages and the same F-key legend. Everything in sections 4-6 applies as
written. The only differences are the ones a browser gives you for free:

- Click a key button (**Enter**, **F1**, **F5**...) or press the key itself.
- Type in an option or pack column on any row; you don't have to tab to it.
- The page fits the window, down to a phone.

There are no pickers, sorting, export or shortcuts here; that's options 10-12.

### Options 10, 11 and 12: the Variety Wholesalers experience

Options 10, 11 and 12 run **the same three programs** in a Variety Wholesalers
browser experience. Everything in sections 4-6 still applies - the same
searches, the same numbers, the same F-keys - with these extras:

| You want to... | Do this |
|---|---|
| Find a SKU, store, department or group without knowing the number | Start typing a number **or a name** in the field; pick from the list (arrow keys + Enter) |
| See every command on the screen | **Ctrl+K** (command palette) |
| See the keyboard shortcuts | **?** |
| Jump to the search or filter box | **/** |
| Sort or filter a table | Click a column heading; type in the filter box (it searches town names too) |
| Take the data to Excel | **Export** (CSV) |
| Previous / next SKU (option 11) | **Prev / Next**, Page Up / Page Down, or Alt+Left / Alt+Right |
| Build a distribution (option 12) | Use the **-/+** steppers or type packs; the totals bar shows SKUs, packs, units and retail as you go. **Smart fill** suggests packs for fast sellers the warehouse can supply. **Create distribution (F10)** shows exactly what will ship before you confirm |
| Read on a dark screen | Moon button or **Alt+T** |
| Use a phone or tablet | It just works: tables turn into cards |

Things that differ from the green screens on purpose:

- The F-keys that open programs missing from the demo (F2-F23 on option 11;
  F3/F4/F6/F7/F8 on option 10) are simply not offered. (Options 7-9 do show
  them, like the green screens, and they do nothing, like the green screens.)
- Option 10 shows the whole list at once and adds **Load more** when the
  program has more stores to page in.
- Option 12's **Top / Bottom** (F7/F8) scroll in the browser instantly.

## 9. Glossary

| Term | Meaning |
|---|---|
| **SKU** | Stock keeping unit: the item number |
| **Chain** | All locations in the company |
| **Region / District** | Groupings of stores for management. Districts belong to regions. |
| **Store group** | A named list of stores used for analysis (e.g. 740 "Sofa Test Stores") |
| **DC / Warehouse** | Distribution center that ships to stores (9001, 9002, ...) |
| **PTD / YTD** | Period to date / year to date (the fiscal period and year) |
| **Pack** | The number of units a warehouse ships together to a store |
| **Pack and hold (P&H)** | Warehouse stock held back for a later release date |
| **Allocated** | Stock committed to store transfers but not yet shipped |
| **Sell-through (%Sold)** | Units sold as a percentage of units sent to the store |
| **Ticketed item** | An item that carries a printed price ticket |
| **Distribution** | A list of items and quantities to ship to a store (created by TS0110 F10) |
| **TY / LY** | This year / last year |

---

## 10. Signing off

1. Leave each program with **F1** until you are back on the menu.
2. Type **90** at *Selection* and press **Enter**.
3. You'll see the *Sign On* screen. You're done.

Please sign off rather than just closing the browser tab, so the demo profile
isn't left signed on.
