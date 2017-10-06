package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.Value
import karazinscalausersgroup.typed.formula.numbers.BuilderSpecification.{`prove builder`, property}
import karazinscalausersgroup.typed.formula.numbers.Number.Zero
import karazinscalausersgroup.typed.formula.numbers.generators.{DoubleValue, IntValue, LongValue}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import karazinscalausersgroup.typed.formula.numbers.builder._
import karazinscalausersgroup.typed.formula.numbers.covering._
import karazinscalausersgroup.typed.formula.numbers.operations._
import karazinscalausersgroup.typed.formula.numbers.replacer._
import karazinscalausersgroup.typed.formula.numbers.{AbstractFoldable, Number}
import shapeless._
import shapeless.ops.hlist._



import scala.language.postfixOps

/**
  * @author Igor Wolkov
  */
object CoveringSpecification extends Properties("CoveringSpecification") {

  /*
                                                 AGENDA

                    |       Principal      |       Interest       |       Fine Fee       |         |
                    |   Debt   | Coverages |   Debt   | Coverages |   Debt   | Coverages | Balance |
    ________________________________________________________________________________________________
    Installment 1   |    -15   |           |    -85   |           |          |           |         |
      (previous)    |          |    15     |          |     75    |          |           |         |
                    |          |           |          |     10    |          |           |    0    |
    ________________________________________________________________________________________________
    Installment 2   |    -30   |           |    -70   |           |    -5    |           |         |
      (previous)    |          |    30     |          |     70    |          |           |    -5   |
    ________________________________________________________________________________________________
    Installment 3   |    -45   |           |    -55   |           |          |           |   -100  |
      (current)     |          |           |          |           |          |           |         |
    ________________________________________________________________________________________________
    Installment 4   |    -60   |           |    -40   |           |          |           |   -100  |
       (next)       |          |           |          |           |          |           |         |
    ________________________________________________________________________________________________
    Installment 5   |    -75   |           |    -25   |           |          |           |   -100  |
       (next)       |          |           |          |           |          |           |         |
    ________________________________________________________________________________________________

    Current balance =
      Σ(Previous Principal + Previous Interest + Previous Fine Fee) +
      Current Principal + Current Interest + Current Fine Fee

    Current balance = -105
    __________________________________________________________________

    Total balance =
      Σ(Previous Principal + Previous Interest + Previous Fine Fee) +
      Current Principal + Current Interest + Current Fine Fee +
      Σ(Next Principal + Next Interest + Next Fine Fee)

    Total balance = -305
    __________________________________________________________________

    Installment term     = 30 days - monthly installment
    Term of usage        = 15 days (for example) - how many days customer uses loan for current installment
    Early repayment rate = 5% (for example) coefficient for the rest of principals

    Early repayment balance =
      Σ(Previous Principal + Previous Interest + Previous Fine Fee) +
      Current Interest + (Term of usage / Installment term) * Current Interest +
      Σ(Next Principal) +
      Early repayment rate * Σ(Next Principal)

    Early repayment balance = -219.25

  */

    // Installment 1 (previous)
    val previousPrincipal1 = PreviousPrincipal(-15L, Covering(15, "Payment #1") :: Nil)
    val previousInterest1 = PreviousInterest(-85L, Covering(75, "Payment #1") :: Covering(10, "Payment #2") :: Nil)
    val installment1 = PreviousInstallment(List(previousInterest1, previousPrincipal1))

    // Installment 2 (previous)
    val previousPrincipal2 = PreviousPrincipal(-30L, Covering(30, "Payment #3") :: Nil)
    val previousInterest2 = PreviousInterest(-70L, Covering(70, "Payment #3") :: Nil)
    val previousFineFee = PreviousFineFee(-5L)
    val installment2 = PreviousInstallment(List(previousInterest2, previousPrincipal2, previousFineFee))

    // Installment 3 (current)
    val currentPrincipal = CurrentPrincipal(-45L)
    val currentInterest = CurrentInterest(-55L)
    val installment3 = CurrentInstallment(List(currentInterest, currentPrincipal))

    // Installment 4 (next)
    val nextPrincipal1 = NextPrincipal(-60L)
    val nextInterest1 = NextInterest(-40L)
    val installment4 = NextInstallment(List(nextInterest1, nextPrincipal1))

    // Installment 5 (next)
    val nextPrincipal2 = NextPrincipal(-75L)
    val nextInterest2 = NextInterest(-25L)
    val installment5 = NextInstallment(List(nextInterest2, nextPrincipal2))

    val loan: Loan = Loan(installment1 :: installment2 :: installment3 :: installment4 :: installment5 :: Nil)

    // Custom coefficient
    // Every coefficient should be defined as new type
    case class `Early repayment rate`(v: Number) extends Value[Number]

    case class `Term of usage`(v: Number) extends Value[Number]

    case class `Installment term`(v: Number) extends Value[Number]

    implicit val termOfUsage: `Term of usage` = `Term of usage`(15)
    implicit val installmentTerm: `Installment term` = `Installment term`(30)
    implicit val `5%`: `Early repayment rate` = `Early repayment rate`(0.05)

    // We don't have PreviousFineFee in all Previous Installments
    // and CurrentFineFee in Current Installment
    // but they potentially can be and
    // and can take part in formulas
    implicit val defaults: List[Value[Number]] =
      List(PreviousFineFee(0), CurrentFineFee(0), termOfUsage, installmentTerm, `5%`)


  property("""|Current balance should be calculated by
              |      Σ(Previous Principal + Previous Interest + Previous Fine Fee)
              |      Current Principal + Current Interest + Current Fine Fee
              |formula""".stripMargin) = {

    implicit val local: Loan = loan

    // Yep, it's like a formula
    // Here `Σ` and `+` are types
    val currentBalance =
      Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
      Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee]

    currentBalance.v == Number(-105)
  }

  property("""|Total balance should be calculated by
              |      Σ(Previous Principal + Previous Interest + Previous Fine Fee) +
              |      Current Principal + Current Interest + Current Fine Fee +
              |      Σ(Next Principal + Next Interest + Next Fine Fee)
              |formula""".stripMargin) = {

    implicit val local: Loan = loan

    val totalBalance =
      Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
      Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee] +
      Σ[NextPrincipal + NextInterest]

    totalBalance.v == Number(-305)
  }

  property("""|Early repayment balance should be calculated by
              |      Σ(Previous Principal + Previous Interest + Previous Fine Fee) +
              |      Current Interest + (Term of usage / Installment term) * Current Interest +
              |      Early repayment rate * Σ(Next Principal)
              |formula""".stripMargin) = {

    implicit val local: Loan = loan

    val earlyRepaymentBalance =
      Σ[PreviousInterest + PreviousPrincipal + PreviousFineFee] +
      Σ[CurrentPrincipal + ((CurrentInterest * `Term of usage`) / `Installment term`) + CurrentFineFee] +
      Σ[NextPrincipal] +
      `5%` * Σ[NextPrincipal]

    println()

    earlyRepaymentBalance.v == Number(-219.25)
  }

  property("Cover current debt Fully") = {

    /*
      How can you see from the early repayment balance formula
      we need to cover only part of amount of CurrentInterest.
      The same for NextPrincipals

      Expression `with value of` restricts the upper limit of coverage.
      It means that if customer need to cover only a half of CurrentInterest
      and the rest of CurrentInterest we cancel you can write

          CurrentInterest `with value of` ~[`1/2` * CurrentInterest]

      Note: you should cover with inverted sign, that's why we use ~[] (it's `tilda`)
      We can't use minus sign `-` instead of `~` because we use `-` for binary minus
    */

    Option(loan) map { implicit loan =>
      val currentBalance =
        Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
        Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee]

      // Don't forget take the value with `-`
      (loan, -currentBalance.v)
    } map {
      case (loan, currentBalance) =>

        val event = LoanEvent(currentBalance, "In Payment #1")

        (loan, event)
    } map {
      case (l, event) =>
        implicit val loan: Loan = l

        val cover =
          Cover[
              (PreviousPrincipal  `with value of` ~[PreviousPrincipal]) ::
              (PreviousInterest   `with value of` ~[PreviousInterest]) ::
              (PreviousFineFee    `with value of` ~[PreviousFineFee]) ::
              (CurrentPrincipal   `with value of` ~[CurrentPrincipal]) ::
              (CurrentInterest    `with value of` ~[CurrentInterest]) ::
              (CurrentFineFee     `with value of` ~[CurrentFineFee]) :: HNil
            ]

        cover.applyEvent[LoanEvent, Covering, InstallmentEntity, Installment, Loan](loan, event, defaults)
    } map {
      case (l, event) =>
        implicit val loan: Loan = l

        val currentBalance =
          Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
          Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee]

        currentBalance.v == Zero &&
          event.description == "In Payment #1" &&
          event.isInstanceOf[EmptyLoanEvent]
    } get
  }

  property("Cover current debt partially") = {

    val delta = -Number(15)

    Option(loan) map { implicit loan =>
      val currentBalance =
        Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
        Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee]

      (loan, -currentBalance.v)
    } map {
      case (loan, currentBalance) =>

        val event = LoanEvent(currentBalance + delta, "In Payment #1")

        (loan, event)
    } map {
      case (l, event) =>
        implicit val loan: Loan = l

        val cover =
          Cover[
            (PreviousPrincipal  `with value of` ~[PreviousPrincipal]) ::
            (PreviousInterest   `with value of` ~[PreviousInterest]) ::
            (PreviousFineFee    `with value of` ~[PreviousFineFee]) ::
            (CurrentPrincipal   `with value of` ~[CurrentPrincipal]) ::
            (CurrentInterest    `with value of` ~[CurrentInterest]) ::
            (CurrentFineFee     `with value of` ~[CurrentFineFee]) :: HNil
          ]

        cover.applyEvent[LoanEvent, Covering, InstallmentEntity, Installment, Loan](loan, event, defaults)
    } map {
      case (l, event) =>
        implicit val loan: Loan = l

        val currentBalance =
          Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
          Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee]

      currentBalance.v == delta &&
        event.description == "In Payment #1" &&
        event.isInstanceOf[EmptyLoanEvent]
    } get
  }

  property("Cover total debt fully") = {

    Option(loan) map { implicit loan =>
      val totalBalance =
        Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
        Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee] +
        Σ[NextPrincipal + NextInterest]

      // Don't forget take the value with `-`
      (loan, -totalBalance.v)
    } map {
      case (loan, currentBalance) =>

        val event = LoanEvent(currentBalance, "In Payment #1")

        (loan, event)
    } map {
      case (l, event) =>
        implicit val loan: Loan = l

        val cover =
          Cover[
              (PreviousPrincipal  `with value of` ~[PreviousPrincipal]) ::
              (PreviousInterest   `with value of` ~[PreviousInterest]) ::
              (PreviousFineFee    `with value of` ~[PreviousFineFee]) ::
              (CurrentPrincipal   `with value of` ~[CurrentPrincipal]) ::
              (CurrentInterest    `with value of` ~[CurrentInterest]) ::
              (CurrentFineFee     `with value of` ~[CurrentFineFee]) ::
              (NextPrincipal      `with value of` ~[NextPrincipal]) ::
              (NextInterest       `with value of` ~[NextInterest]) :: HNil
            ]

        cover.applyEvent[LoanEvent, Covering, InstallmentEntity, Installment, Loan](loan, event, defaults)
    } map {
      case (l, event) =>
        implicit val loan: Loan = l

        val currentBalance =
          Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
            Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee]

        currentBalance.v == Zero &&
          event.description == "In Payment #1" &&
          event.isInstanceOf[EmptyLoanEvent]
    } get
  }

  property("Cover loan with early repayment") = {

    Option(loan) map { implicit loan =>

      val earlyRepaymentFeeAmount = (`5%` * Σ[NextPrincipal]).v

      (loan, earlyRepaymentFeeAmount)

    } map {
      case (loan, earlyRepaymentFeeAmount) =>

        loan.addInstallment(ExtraInstallment(EarlyRepaymentFee(earlyRepaymentFeeAmount) :: Nil))

    } map { implicit loan =>

      val earlyRepaymentAmount =
        Σ[PreviousInterest + PreviousPrincipal + PreviousFineFee] +
        Σ[CurrentPrincipal + ((CurrentInterest * `Term of usage`) / `Installment term`) + CurrentFineFee] +
        Σ[NextPrincipal] +
        Σ[EarlyRepaymentFee]

      (loan, -earlyRepaymentAmount.v)
    } map {
      case (loan, earlyRepaymentAmount) =>

        val event = LoanEvent(earlyRepaymentAmount, "Early repayment Payment #1")

        (loan, event)
    } map {
      case (l, event) =>
        implicit val loan = l

        Cover[
            (PreviousPrincipal  `with value of` ~[PreviousPrincipal]) ::
            (PreviousInterest   `with value of` ~[PreviousInterest]) ::
            (PreviousFineFee    `with value of` ~[PreviousFineFee]) ::
            (CurrentPrincipal   `with value of` ~[CurrentPrincipal]) ::
            (CurrentInterest    `with value of` ~[(CurrentInterest * `Term of usage`) / `Installment term`]) ::
      (NextPrincipal      `with value of` ~[NextPrincipal]) ::
      (EarlyRepaymentFee  `with value of` ~[EarlyRepaymentFee]) :: HNil
      ].applyEvent[LoanEvent, Covering, InstallmentEntity, Installment, Loan](loan, event, defaults)

    } map {
      case (l, _) =>
        implicit val loan = l

        val cancellationAmount =
          Σ[CurrentInterest] + Σ[NextInterest]

        (loan, -cancellationAmount.v)
    } map {
      case (loan, cancellationAmount) =>
        val event = LoanEvent(cancellationAmount, "Early repayment Cancellation #1")

        (loan, event)
    } map {
      case (l, event) =>
        implicit val loan = l

        Cover[
          (CurrentInterest  `with value of` ~[CurrentInterest]) ::
            (NextInterest     `with value of` ~[NextInterest]) :: HNil
          ].applyEvent[LoanEvent, Covering, InstallmentEntity, Installment, Loan](loan, event, defaults)

    } map {
      case (l, _) =>
        implicit val loan = l

        val totalBalance =
          Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
          Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee] +
          Σ[NextPrincipal + NextInterest]

        totalBalance.v == Zero
    } get
  }

}
