
==================== [12] After Simplifier ====================
  

-- RHS size: {terms: 33, types: 24, coercions: 4, joins: 0/0}
lvl_sltw
  = \ @ b_Xl2K ->
      error
        ((PushCallStack
            (build (\ @ b_akVp -> unpackFoldrCString# "error"#))
            (SrcLoc
               (build
                  (\ @ b_akVp -> unpackFoldrCString# "streamly-0.7.1-inplace"#))
               (build
                  (\ @ b_akVp ->
                     unpackFoldrCString# "Streamly.Internal.Data.Stream.StreamD"#))
               (build
                  (\ @ b_akVp ->
                     unpackFoldrCString#
                       "src/Streamly/Internal/Data/Stream/StreamD.hs"#))
               (I# 980#)
               (I# 47#)
               (I# 980#)
               (I# 59#))
            EmptyCallStack)
         `cast` <Co:4>)
        (build (\ @ b_akVp -> unpackFoldrCString# "init"#))

-- RHS size: {terms: 134, types: 171, coercions: 10, joins: 2/2}
$sparselMx'_slwM
  = \ @ s_Xl2G
      @ a_Xl2I
      @ b_Xl2K
      pstep_al2G
      initial_al2H
      ds_al2I
      eta_B1 ->
      case ds_al2I of { UnStream @ s1_al2V step1_al2W state_al3e ->
      case (initial_al2H `cast` <Co:2>) eta_B1 of
      { (# ipv_akT1, ipv1_akT2 #) ->
      joinrec {
        go_slMH ds1_alrd st_alre buf_alrf pst_alrg rval_alrh eta_XZ
          = case ds1_alrd of { __DEFAULT ->
            case pst_alrg of pst1_alrj { __DEFAULT ->
            case ((step1_al2W
                     (State
                        Nothing
                        Nothing
                        defaultMaxThreads
                        defaultMaxThreads
                        Nothing
                        Nothing
                        False)
                     st_alre)
                  `cast` <Co:4>)
                   eta_XZ
            of
            { (# ipv_XkUi, ipv1_XkUk #) ->
            case ipv1_XkUk of {
              Yield x_alrn s2_alro ->
                case ((pstep_al2G pst1_alrj x_alrn) `cast` <Co:4>) ipv_XkUi of
                { (# ipv_XkVf, ipv1_XkVh #) ->
                join {
                  fail_alrr _
                    = case patError
                             "src/Streamly/Internal/Data/Stream/StreamD.hs:(993,17)-(1006,45)|case"#
                      of wild_00 {
                      } } in
                case ipv1_XkVh of {
                  Yield ds3_alrv pst2_alrw b1_alrx ->
                    case ds3_alrv of { I# ds4_alrB ->
                    case ds4_alrB of ds5_alrD {
                      __DEFAULT ->
                        jump go_slMH
                          SPEC
                          s2_alro
                          (case <# 0# ds5_alrD of {
                             __DEFAULT -> [];
                             1# -> $wunsafeTake ds5_alrD (: x_alrn buf_alrf)
                           })
                          pst2_alrw
                          b1_alrx
                          ipv_XkVf;
                      0# -> jump go_slMH SPEC s2_alro [] pst2_alrw b1_alrx ipv_XkVf
                    }
                    };
                  Skip ds3_alrG pst2_alrH ->
                    case ds3_alrG of { I# ds4_alrL ->
                    case ds4_alrL of {
                      __DEFAULT -> jump fail_alrr void#;
                      0# ->
                        jump go_slMH
                          SPEC s2_alro (: x_alrn buf_alrf) pst2_alrH rval_alrh ipv_XkVf
                    }
                    };
                  Stop ds3_alrP b1_alrQ -> (# ipv_XkVf, b1_alrQ #);
                  Error ipv_alrT -> jump fail_alrr void#
                }
                };
              Skip s2_alrW ->
                jump go_slMH SPEC s2_alrW buf_alrf pst1_alrj rval_alrh ipv_XkUi;
              Stop -> (# ipv_XkUi, rval_alrh #)
            }
            }
            }
            }; } in
      jump go_slMH SPEC state_al3e [] ipv1_akT2 lvl_sltw ipv_akT1
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 26, joins: 0/0}
$sparselMx'_sls4 = $sparselMx'_slwM `cast` <Co:26>

-- RHS size: {terms: 5, types: 19, coercions: 0, joins: 0/0}
lvl_slva = \ @ a_Xlfb s_XkXF -> (# s_XkXF, Stop #)

-- RHS size: {terms: 7, types: 19, coercions: 0, joins: 0/0}
lvl_slsK = \ @ a_Xlfb @ r_alfg _ _ _ stp_alfk -> stp_alfk

-- RHS size: {terms: 8, types: 21, coercions: 4, joins: 0/0}
lvl_slvb
  = \ @ a_Xlfb a1_alff s_XkW8 ->
      (# s_XkW8, Yield a1_alff (lvl_slsK `cast` <Co:4>) #)

-- RHS size: {terms: 9, types: 23, coercions: 0, joins: 0/0}
lvl_slvc
  = \ @ a_XlgQ a1_alfd x_alfe s_XkWd ->
      (# s_XkWd, Yield a1_alfd x_alfe #)

-- RHS size: {terms: 8, types: 17, coercions: 32, joins: 0/0}
lvl_slsN
  = \ @ a_XlgV gst_alfb m1_alfc ->
      (m1_alfc `cast` <Co:3>)
        gst_alfb
        (lvl_slvc `cast` <Co:13>)
        (lvl_slvb `cast` <Co:9>)
        (lvl_slva `cast` <Co:7>)

-- RHS size: {terms: 3, types: 8, coercions: 0, joins: 0/0}
$sfromStreamK_slsD = \ @ a_XlgW -> UnStream lvl_slsN

-- RHS size: {terms: 65, types: 153, coercions: 38, joins: 1/3}
$stoStreamK_slxe
  = \ @ a_Xlp2 ds_alp2 @ r_alsH eta_B5 eta_B4 eta_B3 eta_B2 eta_B1 ->
      case ds_alp2 of { UnStream @ s_alp6 step1_alpg state_alph ->
      letrec {
        go_slxd
          = \ st_alp8 @ r_alp9 st1_alpa yld_alpb _ stp_alpd eta_X1s ->
              let {
                lvl_slsO
                  = case st1_alpa of
                    { State ds1_alpl ds2_alps ds3_alpw ds4_alpD ds5_alpE ds6_alpI
                            ds7_alpM ->
                    State Nothing Nothing ds3_alpw ds4_alpD ds5_alpE ds6_alpI ds7_alpM
                    } } in
              joinrec {
                go'_slvh ss_alpf s_akSY
                  = case ((step1_alpg lvl_slsO ss_alpf) `cast` <Co:4>) s_akSY of
                    { (# ipv_akT1, ipv1_akT2 #) ->
                    case ipv1_akT2 of {
                      Yield x_alpR s1_alpS ->
                        ((yld_alpb x_alpR ((go_slxd s1_alpS) `cast` <Co:30>))
                         `cast` <Co:2>)
                          ipv_akT1;
                      Skip s1_alpV -> jump go'_slvh s1_alpV ipv_akT1;
                      Stop -> (stp_alpd `cast` <Co:2>) ipv_akT1
                    }
                    }; } in
              jump go'_slvh st_alp8 eta_X1s; } in
      go_slxd state_alph eta_B5 eta_B4 eta_B3 eta_B2 eta_B1
      }

-- RHS size: {terms: 1, types: 0, coercions: 37, joins: 0/0}
$stoStreamK_slqM = $stoStreamK_slxe `cast` <Co:37>

-- RHS size: {terms: 32, types: 90, coercions: 17, joins: 0/2}
$sunfoldrM_slr9
  = \ @ s_XlqA @ a_XlqD next_aloG state_aloH ->
      let {
        step_slvr
          = \ @ p_aloL _ eta1_aloN s_akSY ->
              case ((next_aloG eta1_aloN) `cast` <Co:5>) s_akSY of
              { (# ipv_akT1, ipv1_akT2 #) ->
              (# ipv_akT1,
                 case ipv1_akT2 of {
                   Nothing -> Stop;
                   Just ds_aloS ->
                     case ds_aloS of { (x_aloW, s1_aloX) -> Yield x_aloW s1_aloX }
                 } #)
              } } in
      let { step_slrb = step_slvr `cast` <Co:12> } in
      UnStream step_slrb state_aloH

-- RHS size: {terms: 44, types: 108, coercions: 39, joins: 0/1}
$sunfoldrM_slNs
  = \ @ b_Xlor
      @ a_Xlot
      step_alor
      seed_alos
      @ r_alsH
      eta_B4
      eta_B3
      eta_B2
      eta_B1
      eta_X2 ->
      letrec {
        go_slNr
          = \ st_alp8 @ r_alp9 _ yld_alpb _ stp_alpd eta_X12 ->
              case ((step_alor st_alp8) `cast` <Co:5>) eta_X12 of
              { (# ipv_akT1, ipv1_akT2 #) ->
              case ipv1_akT2 of {
                Nothing -> (stp_alpd `cast` <Co:2>) ipv_akT1;
                Just ds1_alqY ->
                  case ds1_alqY of { (x_alr2, s1_alr3) ->
                  ((yld_alpb x_alr2 ((go_slNr s1_alr3) `cast` <Co:30>))
                   `cast` <Co:2>)
                    ipv_akT1
                  }
              }
              }; } in
      go_slNr seed_alos eta_B4 eta_B3 eta_B2 eta_B1 eta_X2

-- RHS size: {terms: 1, types: 0, coercions: 50, joins: 0/0}
$sunfoldrM_slsE = $sunfoldrM_slNs `cast` <Co:50>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$sunfoldrMSerial_slsF = $sunfoldrM_slsE

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule_skSD = "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule_skSE = TrNameS $trModule_skSD

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule_skSF = "Main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule_skSG = TrNameS $trModule_skSF

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule_skSE $trModule_skSG

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_sltN = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_sltP = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_sltR = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_sltT = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_sltV = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_sltX = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_sltH = I# 1#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_sltI = I# 1#

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
lvl_sltJ = (lvl_sltH, lvl_sltI)

-- RHS size: {terms: 321, types: 505, coercions: 26, joins: 2/5}
main_slgn
  = \ s_akSY ->
      case parseCLIOpts1 defaultStreamSize s_akSY of
      { (# ipv_akT1, ipv1_akT2 #) ->
      case ipv1_akT2 of { (value_aeNm, cfg_aeNn, benches_aeNo) ->
      let {
        ds_sltM
          = case value_aeNm of { I# ww1_akrv ->
            I# (uncheckedIShiftRA# ww1_akrv 1#)
            } } in
      case value_aeNm of { I# ipv_skVl ->
      case cfg_aeNn of wild_akcW
      { Config ds1_akcZ ds2_akd0 ds3_akd1 ds4_akd2 ds5_akd3 ds6_akd4
               ds7_akd5 ds8_akd6 ds9_akd7 ds10_akd8 ds11_akd9 ds12_akda ds13_akdb
               ds14_akdc ds15_akdd ds16_akde ds17_akdf ds18_akdg ds19_akdh
               ds20_akdj ds21_akdk ds22_akdl ds23_akdm ->
      defaultMain3
        ds22_akdl
        wild_akcW
        benches_aeNo
        (: (BenchGroup
              (build (\ @ b_akVp -> unpackFoldrCString# "o1"#))
              (: (BenchGroup
                    (build (\ @ b_akVp -> unpackFoldrCString# "parser"#))
                    (: (Benchmark
                          (build (\ @ b_akVp -> unpackFoldrCString# "split (all,any)"#))
                          (letrec {
                             go_slod
                               = \ n_akbO eta_X1Z ->
                                   case n_akbO of { I64# x_akUC ->
                                   case <=# x_akUC 0# of {
                                     __DEFAULT ->
                                       case $fRandomInt3 lvl_sltJ eta_X1Z of
                                       { (# ipv_XkUa, ipv1_XkUc #) ->
                                       letrec {
                                         go_slNV
                                           = \ ds1_alrd
                                               st_alre
                                               buf_alrf
                                               pst_alrg
                                               rval_alrh
                                               eta_X2j ->
                                               case ds1_alrd of { __DEFAULT ->
                                               case pst_alrg of pst1_alrj { __DEFAULT ->
                                               case ipv1_XkUc of { I# x_akfI ->
                                               case st_alre of wild_akfp { I# x_akfr ->
                                               case ># x_akfr (+# x_akfI ipv_skVl) of {
                                                 __DEFAULT ->
                                                   join {
                                                     $j_slCZ ipv_XkWy ipv1_XkWA
                                                       = join {
                                                           fail_alrr _
                                                             = case patError
                                                                      "src/Streamly/Internal/Data/Stream/StreamD.hs:(993,17)-(1006,45)|case"#
                                                               of wild_00 {
                                                               } } in
                                                         case ipv1_XkWA of {
                                                           Yield ds3_alrv pst2_alrw b1_alrx ->
                                                             case ds3_alrv of { I# ds4_alrB ->
                                                             case ds4_alrB of ds5_alrD {
                                                               __DEFAULT ->
                                                                 go_slNV
                                                                   SPEC
                                                                   (I# (+# x_akfr 1#))
                                                                   (case <# 0# ds5_alrD of {
                                                                      __DEFAULT -> [];
                                                                      1# ->
                                                                        $wunsafeTake
                                                                          ds5_alrD
                                                                          (: wild_akfp buf_alrf)
                                                                    })
                                                                   pst2_alrw
                                                                   b1_alrx
                                                                   ipv_XkWy;
                                                               0# ->
                                                                 go_slNV
                                                                   SPEC
                                                                   (I# (+# x_akfr 1#))
                                                                   []
                                                                   pst2_alrw
                                                                   b1_alrx
                                                                   ipv_XkWy
                                                             }
                                                             };
                                                           Skip ds3_alrG pst2_alrH ->
                                                             case ds3_alrG of { I# ds4_alrL ->
                                                             case ds4_alrL of {
                                                               __DEFAULT -> jump fail_alrr void#;
                                                               0# ->
                                                                 go_slNV
                                                                   SPEC
                                                                   (I# (+# x_akfr 1#))
                                                                   (: wild_akfp buf_alrf)
                                                                   pst2_alrH
                                                                   rval_alrh
                                                                   ipv_XkWy
                                                             }
                                                             };
                                                           Stop ds3_alrP b1_alrQ ->
                                                             (# ipv_XkWy, b1_alrQ #);
                                                           Error ipv_alrT -> jump fail_alrr void#
                                                         } } in
                                                   case pst1_alrj of {
                                                     SeqParseL st_akgH ->
                                                       jump $j_slCZ
                                                         eta_X2j
                                                         (case st_akgH of {
                                                            False ->
                                                              Skip
                                                                lvl_sltN
                                                                (SeqParseR ((,) False) False);
                                                            True ->
                                                              case ds_sltM of { I# y_akrL ->
                                                              case <=# x_akfr y_akrL of {
                                                                __DEFAULT ->
                                                                  Skip
                                                                    lvl_sltP
                                                                    (SeqParseR ((,) False) False);
                                                                1# -> Skip lvl_sltR (SeqParseL True)
                                                              }
                                                              }
                                                          });
                                                     SeqParseR f_akh3 st_akh4 ->
                                                       jump $j_slCZ
                                                         eta_X2j
                                                         (case st_akh4 of {
                                                            False ->
                                                              case ># x_akfr ipv_skVl of {
                                                                __DEFAULT ->
                                                                  Yield
                                                                    lvl_sltT
                                                                    (SeqParseR f_akh3 False)
                                                                    (f_akh3 False);
                                                                1# -> Stop lvl_sltV (f_akh3 True)
                                                              };
                                                            True -> Stop lvl_sltX (f_akh3 True)
                                                          })
                                                   };
                                                 1# -> (# eta_X2j, rval_alrh #)
                                               }
                                               }
                                               }
                                               }
                                               }; } in
                                       case go_slNV
                                              SPEC
                                              ipv1_XkUc
                                              []
                                              (SeqParseL True)
                                              (error
                                                 ((PushCallStack
                                                     (build
                                                        (\ @ b_akVp ->
                                                           unpackFoldrCString# "error"#))
                                                     (SrcLoc
                                                        (build
                                                           (\ @ b_akVp ->
                                                              unpackFoldrCString#
                                                                "streamly-0.7.1-inplace"#))
                                                        (build
                                                           (\ @ b_akVp ->
                                                              unpackFoldrCString#
                                                                "Streamly.Internal.Data.Stream.StreamD"#))
                                                        (build
                                                           (\ @ b_akVp ->
                                                              unpackFoldrCString#
                                                                "src/Streamly/Internal/Data/Stream/StreamD.hs"#))
                                                        (I# 980#)
                                                        (I# 47#)
                                                        (I# 980#)
                                                        (I# 59#))
                                                     EmptyCallStack)
                                                  `cast` <Co:4>)
                                                 (build
                                                    (\ @ b_akVp -> unpackFoldrCString# "init"#)))
                                              ipv_XkUa
                                       of
                                       { (# ipv_akbY, ipv1_akbZ #) ->
                                       case seq#
                                              (case ipv1_akbZ of { (x_akV5, y_akV6) ->
                                               case x_akV5 of { __DEFAULT ->
                                               case y_akV6 of { __DEFAULT -> () }
                                               }
                                               })
                                              ipv_akbY
                                       of
                                       { (# ipv2_akc3, ipv3_akc4 #) ->
                                       go_slod (I64# (-# x_akUC 1#)) ipv2_akc3
                                       }
                                       }
                                       };
                                     1# -> (# eta_X1Z, () #)
                                   }
                                   }; } in
                           Benchmarkable
                             ((\ ds_akce -> ds_akce) `cast` <Co:3>)
                             ((\ _ eta_X20 -> (# eta_X20, () #)) `cast` <Co:5>)
                             ((\ _ _ eta_X41 -> (# eta_X41, () #)) `cast` <Co:7>)
                             ((\ _ eta_X21 eta_X41 -> go_slod eta_X21 eta_X41) `cast` <Co:7>)
                             False))
                       []))
                 []))
           [])
        ipv_akT1
      }
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
main = main_slgn `cast` <Co:3>

-- RHS size: {terms: 2, types: 1, coercions: 3, joins: 0/0}
main_slnC = runMainIO1 (main_slgn `cast` <Co:3>)

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
main = main_slnC `cast` <Co:3>


------ Local rules for imported ids --------
"SPEC/Main fromStreamK @ IO _" [0]
    forall @ a_alf9 $dMonad_sloi.
      fromStreamK $dMonad_sloi
      = $sfromStreamK_slsD
"SPEC/Main toStreamK @ IO _" [0]
    forall @ a_alp0 $dMonad_slq5.
      toStreamK $dMonad_slq5
      = $stoStreamK_slqM
"SPEC/Main unfoldrM @ IO _ _" [1]
    forall @ s_aloD @ a_aloE $dMonad_slr8.
      unfoldrM $dMonad_slr8
      = $sunfoldrM_slr9
"SPEC/Main unfoldrM @ SerialT @ IO _ _"
    forall @ b_alon @ a_aloo $dMonad_Xlr6 $dIsStream_sloA.
      unfoldrM $dIsStream_sloA $dMonad_Xlr6
      = $sunfoldrM_slsE
"SPEC/Main unfoldrMSerial @ IO _ _" [2]
    forall @ b_alfq @ a_alfr $d(%,,%)_slok.
      unfoldrMSerial $d(%,,%)_slok
      = $sunfoldrMSerial_slsF
"SPEC/Main parselMx' @ IO _ _ _" [1]
    forall @ s_al2C @ a_al2D @ b_al2E $dMonad_sls3.
      parselMx' $dMonad_sls3
      = $sparselMx'_sls4

