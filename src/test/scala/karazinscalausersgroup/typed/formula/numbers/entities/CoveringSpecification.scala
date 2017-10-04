package karazinscalausersgroup.typed.formula.numbers.entities

import karazinscalausersgroup.typed.formula.Value
import karazinscalausersgroup.typed.formula.numbers.BuilderSpecification.{`prove builder`, property}
import karazinscalausersgroup.typed.formula.numbers.Number
import karazinscalausersgroup.typed.formula.numbers.generators.{DoubleValue, IntValue, LongValue}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import karazinscalausersgroup.typed.formula.numbers.builder._
//import karazinscalausersgroup.typed.formula.accounting.covering4._
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
    Early repayment rate = 3% (for example) coefficient for the rest of principals

    Early repayment balance =
      Σ(Previous Principal + Previous Interest + Previous Fine Fee) +
      Current Interest + (Term of usage / Installment term) * Current Interest +
      Early repayment rate * Σ(Next Principal)

    Early repayment balance = -81.55

  */

  object LoanContext {
    // Installment 1 (previous)
    val previousPrincipal1 = PreviousPrincipal(-15L, Covering(15) :: Nil)
    val previousInterest1 = PreviousInterest(-85L, Covering(75) :: Covering(10) :: Nil)
    val installment1 = PreviousInstallment(List(previousInterest1, previousPrincipal1))

    // Installment 2 (previous)
    val previousPrincipal2 = PreviousPrincipal(-30L, Covering(30) :: Nil)
    val previousInterest2 = PreviousInterest(-70L, Covering(70) :: Nil)
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

    implicit val loan: Loan = Loan(installment1 :: installment2 :: installment3 :: installment4 :: installment5 :: Nil)

    // Custom coefficient
    // Every coefficient should be defined as new type
    case class `Early repayment rate`(v: Number) extends Value[Number]

    case class `Term of usage`(v: Number) extends Value[Number]

    case class `Installment term`(v: Number) extends Value[Number]

    implicit val termOfUsage: `Term of usage` = `Term of usage`(15)
    implicit val installmentTerm: `Installment term` = `Installment term`(30)
    implicit val `3%`: `Early repayment rate` = `Early repayment rate`(0.03)

    // We don't have PreviousFineFee in all Previous Installments
    // and CurrentFineFee in Current Installment
    // but they potentially can be and
    // and can take part in formulas
    val defaults: Seq[Value[Number]] =
    Seq(PreviousFineFee(0), CurrentFineFee(0))

    implicit val context: Seq[Value[Number]] =
      defaults ++ Seq(termOfUsage, installmentTerm, `3%`)
  }

  property("""|Current balance should be calculated by
              |      Σ(Previous Principal + Previous Interest + Previous Fine Fee)
              |      Current Principal + Current Interest + Current Fine Fee
              |formula""".stripMargin) = {
    import LoanContext._

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
    import LoanContext._

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
    import LoanContext._

    val earlyRepaymentBalance =
      Σ[PreviousInterest + PreviousPrincipal + PreviousFineFee] +
      Σ[CurrentPrincipal + ((CurrentInterest * `Term of usage`) / `Installment term`) + CurrentFineFee] +
      `3%` * Σ[NextPrincipal]

    earlyRepaymentBalance.v == Number(-81.55)
  }

  property("""Cover Current Installment Partially""".stripMargin) = {

    /*
      How can you see from the early repayment balance formula
      we need to cover only part of amount of CurrentInterest.
      The same for NextPrincipals

      Expression `with value of` restricts the upper limit of coverage.
      It means that if customer need to cover only a half of CurrentInterest
      and the rest of CurrentInterest we cancel you can write

          CurrentInterest `with value of` ~[`1/2` * CurrentInterest]

      Note: you should cover with inversed sign, that's why we use ~[] (it's `tilda`)
      We can't use minus sign `-` instead of `~` because we use `-` for binary minus
      */


    // We need to separate spaces because we use
    // implicit value of Loan
    def `covering space`: (Loan, LoanEvent) = {
      import LoanContext._

      val cover =
        Cover[
          (CurrentPrincipal `with value of` ~[CurrentPrincipal]) ::
          (CurrentInterest `with value of` ~[CurrentInterest]) :: HNil
        ]

      val event = LoanEvent(Number(90L), "In Payment #1")

      // Cover CurrentPrincipal and CurrentInterest with event
      // Unfortunately for now we have to list all generic types
      // But we are working on how to get rid of them
      cover.applyEvent[LoanEvent, Covering, InstallmentEntity, Installment, Loan](loan, event, defaults)
    }

    def `recalculations space`(newContext: (Loan, LoanEvent)): Boolean = {
      import LoanContext.context
      implicit val (updatedLoan, updatedEvent) = newContext

      val currentBalance =
        Σ[PreviousPrincipal + PreviousInterest + PreviousFineFee] +
        Σ[CurrentPrincipal + CurrentInterest + CurrentFineFee]

      // We forgot to cover Previous Fine Fee (-5) from Previous Installment
      // and we don't have enough money to cover CurrentInterest wholly
      currentBalance.v == Number(-15) &&
      updatedEvent.description == "In Payment #1" &&
      updatedEvent.isInstanceOf[EmptyLoanEvent]
    }

    `recalculations space`(`covering space`)
  }

  property("""Cover Loan with Early Repayment and check new loan structure""".stripMargin) = {

    import LoanContext._

    val cover =
      Cover[
        (PreviousPrincipal  `with value of` ~[PreviousPrincipal]) ::
        (PreviousInterest   `with value of` ~[PreviousInterest]) ::
        (PreviousFineFee    `with value of` ~[PreviousFineFee]) ::
        (CurrentPrincipal   `with value of` ~[CurrentPrincipal]) ::
        (CurrentInterest    `with value of` ~[(CurrentInterest * `Term of usage`) / `Installment term`]) ::
        (NextPrincipal      `with value of` ~[`Early repayment rate` * NextPrincipal]) :: HNil
      ]

    val event = LoanEvent(Number(90L), "In Payment #2")

    val (updatedLoan, updatedEvent) = cover.applyEvent[LoanEvent, Covering, InstallmentEntity, Installment, Loan](loan, event, context)

    // Review lines with note /*New covering*/
    // Installment 1 (previous)
    val previousPrincipal1 = PreviousPrincipal(-15L, Covering(15) :: Nil)
    val previousInterest1 = PreviousInterest(-85L, Covering(75) :: Covering(10) :: Nil)
    val installment1 = PreviousInstallment(List(previousInterest1, previousPrincipal1))

    // Installment 2 (previous)
    val previousPrincipal2 = PreviousPrincipal(-30L, Covering(30) :: Nil)
    val previousInterest2 = PreviousInterest(-70L, Covering(70) :: Nil)
    val previousFineFee = PreviousFineFee(-5L, /*New covering*/Covering(5L) :: Nil)
    val installment2 = PreviousInstallment(List(previousInterest2, previousPrincipal2, previousFineFee))

    // Installment 3 (current)
    val currentPrincipal = CurrentPrincipal(-45L, /*New covering*/Covering(45L) :: Nil)
    val currentInterest = CurrentInterest(-55L, /*New covering*/Covering(27.5) :: Nil)
    val installment3 = CurrentInstallment(List(currentInterest, currentPrincipal))

    // Installment 4 (next)
    val nextPrincipal1 = NextPrincipal(-60L, /*New covering*/Covering(1.7999999999999998) :: Nil)
    val nextInterest1 = NextInterest(-40L)
    val installment4 = NextInstallment(List(nextInterest1, nextPrincipal1))

    // Installment 5 (next)
    val nextPrincipal2 = NextPrincipal(-75L, /*New covering*/Covering(2.25) :: Nil)
    val nextInterest2 = NextInterest(-25L)
    val installment5 = NextInstallment(List(nextInterest2, nextPrincipal2))

    val expectedLoan: Loan = Loan(installment1 :: installment2 :: installment3 :: installment4 :: installment5 :: Nil)

    // Verify results
    expectedLoan == updatedLoan &&
      // 8.45 = 90 - Early Repayment amount. Lets check in the next test
      updatedEvent == NonEmptyLoanEvent(Number(8.45), "In Payment #2")
  }


  property("""Cover Loan with Early Repayment and check event calculations""".stripMargin) = {
    import LoanContext._

    val earlyRepaymentAmount =
      Σ[PreviousInterest + PreviousPrincipal + PreviousFineFee] +
      Σ[CurrentPrincipal + ((CurrentInterest * `Term of usage`) / `Installment term`) + CurrentFineFee] +
      `3%` * Σ[NextPrincipal]

    val event = LoanEvent(Number(90L), "In Payment #2")

    val cover =
      Cover[
        (PreviousPrincipal  `with value of` ~[PreviousPrincipal]) ::
        (PreviousInterest   `with value of` ~[PreviousInterest]) ::
        (PreviousFineFee    `with value of` ~[PreviousFineFee]) ::
        (CurrentPrincipal   `with value of` ~[CurrentPrincipal]) ::
        (CurrentInterest    `with value of` ~[(CurrentInterest * `Term of usage`) / `Installment term`]) ::
        (NextPrincipal      `with value of` ~[`Early repayment rate` * NextPrincipal]) :: HNil
    ]

    val (_, updatedEvent) = cover.applyEvent[LoanEvent, Covering, InstallmentEntity, Installment, Loan](loan, event, context)

    // Check result
    updatedEvent match {
      case NonEmptyLoanEvent(value, _) =>
        // TODO: Fix magic number
        // Issue with double precision
        value + Number(0.000000000000003) == event.value + earlyRepaymentAmount.v

      case EmptyLoanEvent(_)           => false
    }
  }

}
