(defpackage #:financial.statements
  (:use #:cl)
  (:export #:BsTyp #:PlTyp #:CfTyp)
  (:local-nicknames (#:sp #:serapeum)))

(in-package #:financial.statements)

(deftype BsTyp ()
  '(member
    :Cash
    :CurrentReceivables
    :CurrentLoans
    :CurrentAdvances
    :OtherCurrentAssets
    :CurrentInvestments
    :Inventories
    :RawMaterials
    :WorkInProgress
    :FinishedGoods
    :CurrentAssets
    :AccountReceivables
    :LongTermLoanAssets
    :LongTermAdvances
    :LongTermInvestments
    :OtherLongTermAssets
    :PlantPropertyEquipment
    :AccumulatedDepreciation
    :NetPlantPropertyEquipment
    :LeasingRentalAssets
    :AccumulatedAmortizationLease
    :NetLeaseRentalAssets
    :Goodwill
    :CapitalWip
    :OtherTangibleAssets
    :IntangibleAssets
    :IntangibleAssetsDevelopment
    :AccumulatedAmortization
    :NetIntangibleAssets
    :LongTermAssets
    :Assets
    :CurrentPayables
    :CurrentBorrowings
    :CurrentNotesPayable
    :OtherCurrentLiabilities
    :InterestPayable
    :CurrentProvisions
    :CurrentTaxPayables
    :LiabilitiesSaleAssets
    :CurrentLeasesLiability
    :CurrentLiabilities
    :AccountPayables
    :LongTermBorrowings
    :BondsPayable
    :DeferredTaxLiabilities
    :LongTermLeasesLiability
    :DeferredCompensation
    :DeferredRevenues
    :CustomerDeposits
    :OtherLongTermLiabilities
    :PensionProvision
    :TaxProvision
    :LongTermProvision
    :LongTermLiabilities
    :Liabilities
    :CommonStock
    :PreferredStock
    :PdInCapAbovePar
    :PdInCapTreasuryStock
    :RevaluationReserves
    :Reserves
    :RetainedEarnings
    :AccumulatedOCI
    :MinorityInterests
    :Equity
    :BalanceSheetCheck))

(deftype PlTyp ()
  '(member
    :OperatingRevenue
    :NonOperatingRevenue
    :ExciseStaxLevy
    :OtherIncome
    :Revenue
    :CostMaterial
    :DirectExpenses
    :COGS
    :Salaries
    :AdministrativeExpenses
    :ResearchNDevelopment
    :OtherOverheads
    :OtherOperativeExpenses
    :OtherExpenses
    :ExceptionalItems
    :GrossProfit
    :EBITDA
    :Depreciation
    :TaxDepreciation
    :AssetImpairment
    :LossDivestitures
    :Amortization
    :EBITX
    :InterestRevenue
    :InterestExpense
    :CostDebt
    :OtherFinancialRevenue
    :EBTX
    :ExtraordinaryItems
    :PriorYears
    :EBT
    :TaxesCurrent
    :TaxesDeferred
    :EAT
    :NetIncomeDiscontinuedOps
    :NetIncome
    :Dividends
    :ContributionRetainedEarnings
    :GainsLossesForex
    :GainsLossesActurial
    :GrossSalesPPE
    :GrossSalesLeaseRentalAssets
    :GrossSalesIntangibleAssets
    :AccAmortSalesPPE
    :AccAmortSalesLeaseRental
    :AccAmortSalesIntangible
    :SalesAmountPPE
    :SalesAmountLeaseRentalAssets
    :SalesAmountIntangibleAssets
    :GainsLossesSales
    :FvChangeAvlSale
    :OtherDeferredTaxes
    :OtherComprehensiveIncome
    :TotalComprehensiveIncome))

(deftype CfTyp ()
  '(member
    :ChangeCurrentAssets
    :ChangeLongTermAssets
    :ChangeCurrentLiabilities
    :ChangeLongTermLiabilities
    :ChangeProvisions
    :ChangeRetainedEarnings
    :AdjustmentsRetainedEarnings
    :ChangeAccumulatedOci
    :OtherCashFlowOperations
    :CashFlowOperations
    :ChangePPE
    :ChangeReserves
    :AdjustmentsSalesAssets
    :InvestmentsCapDevp
    :InvestmentsLoans
    :ChangeEquityAssets
    :ChangeInvestments
    :OtherCashFlowInvestments
    :CashFlowInvestments
    :StockSalesAndPurchase
    :ChangeDebt
    :CashFlowInterests
    :CashFlowDividends
    :DonorContribution
    :OtherCashFlowFinancing
    :CashFlowFinancing
    :NetCashFlow
    :FreeCashFlowFirm
    :CashFlowTaxShield
    :FreeCashFlowEquity
    :CashFlowDebt))

(deftype FinOtherType ()
  '(member
    :CorporateTaxRate
    :GrossProfitTaxRate
    :RevenueTaxRate
    :CurrentRatio
    :AcidRatio
    :DaysOfInventory
    :InventoryTurnoverRatio))

(defparameter *balance_sheet_map*
  (list
   (list :Inventories
         (list (list :RawMaterials :WorkInProgress :FinishedGoods) ()))
   (list :CurrentAssets
         (list (list
                :Cash
                :CurrentReceivables
                :CurrentLoans
                :CurrentAdvances
                :OtherCurrentAssets
                :CurrentInvestments
                :Inventories)
               ()))
   (list :NetPlantPropertyEquipment
         (list (list :PlantPropertyEquipment) (list :AccumulatedDepreciation)))
   (list :NetLeaseRentalAssets
         (list (list :LeasingRentalAssets) (list :AccumulatedAmortizationLease)))
   (list :NetIntangibleAssets
         (list (list :IntangibleAssets :IntangibleAssetsDevelopment)
               (list :AccumulatedAmortization)))
   (list :LongTermAssets
         (list (list
                :AccountReceivables
                :LongTermLoanAssets
                :LongTermAdvances
                :LongTermInvestments
                :OtherLongTermAssets
                :NetPlantPropertyEquipment
                :NetLeaseRentalAssets
                :Goodwill
                :CapitalWip
                :OtherTangibleAssets
                :NetIntangibleAssets)
               ()))
   (list :Assets
         (list (list :CurrentAssets :LongTermAssets) ()))
   (list :CurrentLiabilities
         (list (list
                :CurrentPayables
                :CurrentBorrowings
                :CurrentNotesPayable
                :OtherCurrentLiabilities
                :InterestPayable
                :CurrentProvisions
                :CurrentTaxPayables
                :LiabilitiesSaleAssets
                :CurrentLeasesLiability)
               ()))
   (list :LongTermLiabilities
         (list (list
                :AccountPayables
                :LongTermBorrowings
                :BondsPayable
                :DeferredTaxLiabilities
                :LongTermLeasesLiability
                :DeferredCompensation
                :DeferredRevenues
                :CustomerDeposits
                :OtherLongTermLiabilities
                :PensionProvision
                :TaxProvision
                :LongTermProvision)
               ()))
   (list :Liabilities (list (list :CurrentLiabilities :LongTermLiabilities) ()))
   (list :Equity
         (list (list
                :CommonStock
                :PreferredStock
                :PdInCapAbovePar
                :PdInCapTreasuryStock
                :RevaluationReserves
                :Reserves
                :RetainedEarnings
                :AccumulatedOCI
                :MinorityInterests)
               ()))
   (list :BalanceSheetCheck (list (list :Assets) (list :Liabilities :Equity)))))

(defparameter *profit_loss_map*
  (list
   (list :Revenue
         (list (list :OperatingRevenue :NonOperatingRevenue) (list :ExciseStaxLevy)))
   (list :COGS (list (list :CostMaterial :DirectExpenses) ()))
   (list :GrossProfit (list (list :Revenue) (list :COGS)))
   (list :EBITDA
         (list (list :GrossProfit :OtherIncome)
               (list :Salaries
                     :AdministrativeExpenses
                     :ResearchNDevelopment
                     :OtherOverheads
                     :OtherOperativeExpenses
                     :OtherExpenses
                     :ExceptionalItems)))
   (list :EBITX
         (list (list :EBITDA)
               (list :Depreciation :AssetImpairment :LossDivestitures :Amortization)))
   (list :EBTX
         (list (list :EBITX :InterestRevenue :OtherFinancialRevenue)
               (list :InterestExpense :CostDebt)))
   (list :EBT (list (list :EBTX) (list :ExtraordinaryItems :PriorYears)))
   (list :EAT (list (list :EBT) (list :TaxesCurrent :TaxesDeferred)))
   (list :NetIncome (list (list :EAT :NetIncomeDiscontinuedOps) ()))
   (list :ContributionRetainedEarnings (list (list :NetIncome) (list :Dividends)))
   (list :GainsLossesSales
         (list (list
                :SalesAmountPPE
                :SalesAmountLeaseRentalAssets
                :SalesAmountIntangibleAssets
                :AccAmortSalesPPE
                :AccAmortSalesLeaseRental
                :AccAmortSalesIntangible)
               (list :GrossSalesPPE
                     :GrossSalesLeaseRentalAssets
                     :GrossSalesIntangibleAssets)))
   (list :OtherComprehensiveIncome
         (list (list :GainsLossesForex
                     :GainsLossesActurial
                     :GainsLossesSales
                     :FvChangeAvlSale)
               (list :OtherDeferredTaxes)))
   (list :TotalComprehensiveIncome (list (list :NetIncome :OtherComprehensiveIncome) ()))))

(defparameter *cash_flow_map*
  (list
   (list :CashFlowOperations
         (list (list
                :ChangeCurrentLiabilities
                :ChangeLongTermLiabilities
                :ChangeProvisions
                :ChangeRetainedEarnings
                :AdjustmentsRetainedEarnings
                :ChangeAccumulatedOci
                :OtherCashFlowOperations)
               (list :ChangeCurrentAssets :ChangeLongTermAssets)))
   (list :CashFlowInvestments
         (list (list
                :OtherCashFlowInvestments
                :AdjustmentsSalesAssets
                :ChangeReserves)
               (list
                :ChangePPE
                :InvestmentsCapDevp
                :InvestmentsLoans
                :ChangeEquityAssets
                :ChangeInvestments)))
   (list :CashFlowFinancing
         (list (list
                :StockSalesAndPurchase
                :ChangeDebt :DonorContribution
                :OtherCashFlowFinancing)
               (list :CashFlowInterests :CashFlowDividends)))
   (list :NetCashFlow
         (list (list :CashFlowOperations :CashFlowInvestments :CashFlowFinancing) ()))
   (list :FreeCashFlowEquity
         (list (list
                :CashFlowOperations
                :ChangeDebt
                :ChangeReserves
                :AdjustmentsSalesAssets)
               (list :CashFlowInterests :ChangePPE)))
   (list :CashFlowDebt (list (list :CashFlowInterests) (list :ChangeDebt)))
   (list :FreeCashFlowFirm
         (list (list :FreeCashFlowEquity :CashFlowDebt) (list :CashFlowTaxShield)))))


(defparameter *cash_flow_balance_sheet*
  (list
   (list :ChangeCurrentAssets
         (list (list
                :CurrentReceivables
                :CurrentLoans
                :CurrentAdvances
                :OtherCurrentAssets
                :CurrentInvestments
                :RawMaterials
                :WorkInProgress
                :FinishedGoods)
               ()))
   (list :ChangeLongTermAssets
         (list (list :AccountReceivables :LongTermAdvances :CapitalWip)
               (list
                :AccumulatedDepreciation
                :AccumulatedAmortizationLease
                :AccumulatedAmortization)))
   (list :ChangeCurrentLiabilities
         (list (list
                :CurrentPayables
                :CurrentBorrowings
                :CurrentNotesPayable
                :OtherCurrentLiabilities
                :InterestPayable
                :CurrentProvisions
                :CurrentTaxPayables
                :LiabilitiesSaleAssets
                :CurrentLeasesLiability)
               ()))
   (list :ChangeLongTermLiabilities
         (list (list
                :AccountPayables
                :DeferredTaxLiabilities
                :DeferredCompensation
                :DeferredRevenues
                :CustomerDeposits
                :OtherLongTermLiabilities)
               ()))
   (list :ChangeProvisions
         (list (list :PensionProvision :TaxProvision :LongTermProvision) ()))
   (list :ChangeRetainedEarnings (list (list :RetainedEarnings) ()))
   (list :ChangeAccumulatedOci (list (list :AccumulatedOCI) ()))
   (list :ChangePPE (list (list :PlantPropertyEquipment :LeasingRentalAssets) ()))
   (list :ChangeReserves (list (list :RevaluationReserves :Reserves) ()))
   (list :InvestmentsCapDevp (list (list :IntangibleAssetsDevelopment) ()))
   (list :InvestmentsLoans (list (list :LongTermLoanAssets) ()))
   (list :ChangeEquityAssets (list (list :IntangibleAssets) ()))
   (list :ChangeInvestments (list (list :LongTermInvestments :Goodwill) ()))
   (list :OtherCashFlowInvestments
         (list () (list :OtherLongTermAssets :OtherTangibleAssets)))
   (list :StockSalesAndPurchase
         (list (list
                :CommonStock
                :PreferredStock
                :PdInCapAbovePar
                :PdInCapTreasuryStock)
               ()))
   (list :ChangeDebt
         (list (list :LongTermBorrowings :BondsPayable :LongTermLeasesLiability) ()))
   (list :OtherCashFlowFinancing (list (list :MinorityInterests) ()))))

(defparameter *cash_flow_profit_loss*
  (list
   (list :CashFlowInterests (list (list :InterestExpense :CostDebt) ()))
   (list :CashFlowDividends (list (list :Dividends) ()))
   (list :AdjustmentsRetainedEarnings
         (list (list
                :InterestExpense
                :CostDebt
                :Dividends
                :AccAmortSalesPPE
                :AccAmortSalesLeaseRental
                :AccAmortSalesIntangible)
               (list :GainsLossesSales)))
   (list :AdjustmentsSalesAssets
         (list (list :GainsLossesSales)
               (list
                :AccAmortSalesPPE
                :AccAmortSalesLeaseRental
                :AccAmortSalesIntangible)))))
