{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad (void)

-- Plutus
import Ledger
import Ledger.Ada as Ada
import Plutus.Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet

-- Your on-chain code
import PublicFund

------------------------------------------------------------
-- Schema
------------------------------------------------------------

type EscrowSchema =
        Endpoint "lock" ()
    .\/ Endpoint "approve" ()
    .\/ Endpoint "release" ()
    .\/ Endpoint "refund" ()

------------------------------------------------------------
-- Script address
------------------------------------------------------------

escrowAddress :: Address
escrowAddress = scriptAddress validator

------------------------------------------------------------
-- Datum helpers
------------------------------------------------------------

mkDatum :: [PaymentPubKeyHash] -> EscrowDatum
mkDatum approvals =
    EscrowDatum
        { edDepositor   = mockWalletPaymentPubKeyHash $ knownWallet 1
        , edBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
        , edOfficials   =
            fmap mockWalletPaymentPubKeyHash
                [ knownWallet 3
                , knownWallet 4
                ]
        , edApprovals   = approvals
        , edRequired    = 2
        , edDeadline    = 20_000
        }

------------------------------------------------------------
-- Lock funds
------------------------------------------------------------

lock :: Contract () EscrowSchema Text ()
lock = do
    let datum = mkDatum []
        tx =
            mustPayToTheScript datum
                (Ada.lovelaceValueOf 10_000_000)

    void $ submitTxConstraints validator tx

------------------------------------------------------------
-- Approve (updates datum!)
------------------------------------------------------------

approve :: Wallet -> Contract () EscrowSchema Text ()
approve w = do
    utxos <- utxosAt escrowAddress
    case utxos of
        [(oref, o)] -> do
            let oldDatum =
                    case _ciTxOutDatum o of
                        Left _          -> traceError "no datum"
                        Right (Datum d) -> unsafeFromBuiltinData d

                signer = mockWalletPaymentPubKeyHash w

                newDatum =
                    oldDatum
                        { edApprovals =
                            signer : edApprovals oldDatum
                        }

                tx =
                    mustSpendScriptOutput oref
                        (Redeemer $ toBuiltinData Approve)
                    <> mustPayToTheScript newDatum
                        (Ada.lovelaceValueOf 10_000_000)

            void $ submitTxConstraintsSpending validator utxos tx

        _ -> logError @String "expected exactly one script UTxO"

------------------------------------------------------------
-- Release
------------------------------------------------------------

release :: Contract () EscrowSchema Text ()
release = do
    utxos <- utxosAt escrowAddress
    case utxos of
        [(oref, _)] ->
            void $
              submitTxConstraintsSpending
                validator
                utxos
                (mustSpendScriptOutput oref
                    (Redeemer $ toBuiltinData Release))
        _ -> logError @String "no script utxo"

------------------------------------------------------------
-- Refund
------------------------------------------------------------

refund :: Contract () EscrowSchema Text ()
refund = do
    utxos <- utxosAt escrowAddress
    case utxos of
        [(oref, _)] ->
            void $
              submitTxConstraintsSpending
                validator
                utxos
                (mustSpendScriptOutput oref
                    (Redeemer $ toBuiltinData Refund))
        _ -> logError @String "no script utxo"

------------------------------------------------------------
-- Emulator trace
------------------------------------------------------------

trace :: EmulatorTrace ()
trace = do
    -- Wallet 1 locks funds
    h1 <- activateContractWallet (knownWallet 1) lock
    void $ Emulator.waitNSlots 1

    -- Officials approve
    h3 <- activateContractWallet (knownWallet 3) (approve $ knownWallet 3)
    void $ Emulator.waitNSlots 1

    h4 <- activateContractWallet (knownWallet 4) (approve $ knownWallet 4)
    void $ Emulator.waitNSlots 1

    -- Beneficiary releases
    h2 <- activateContractWallet (knownWallet 2) release
    void $ Emulator.waitNSlots 1

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main =
    runEmulatorTraceIO trace
