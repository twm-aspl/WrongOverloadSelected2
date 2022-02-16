///
/// Regression test for #2809
///
module GenericsMatching

open Fable.Core.Testing
open Optics
open Optics.Operators

//
// Types with required nesting and optics to reproduce error
//
type private Result<'T> =
    | Ok of 'T
    | Error of string

type private PaymentFrequencyDto =
    | Weekly = 1
    | Fortnightly = 2
    | Monthly = 3
    | Quarterly = 4
    | Annual = 5

type private InsurancePolicyDetailsDto = {
    PaymentFrequency : PaymentFrequencyDto
    Discount : decimal option
} with
    static member discount_ : Prism<InsurancePolicyDetailsDto, decimal> =
        (fun p -> p.Discount),
        (fun x p ->
            match p with
            | p when p.Discount.IsSome -> {p with Discount = Some x}
            | p -> p)
    static member paymentFrequency_ : Lens<InsurancePolicyDetailsDto, PaymentFrequencyDto> =
        (fun p -> p.PaymentFrequency), (fun x p -> { p with PaymentFrequency = x })

type private AccountDetailsDto = {
    Tag : string
    InsurancePolicyData : InsurancePolicyDetailsDto option
} with
    static member insurancePolicyData_ : Prism<AccountDetailsDto, InsurancePolicyDetailsDto> =
        (fun p -> p.InsurancePolicyData),
        (fun x p ->
            match p with
            | p when p.InsurancePolicyData.IsSome -> {p with InsurancePolicyData = Some x}
            | p -> p)


type private AccountDto = {
    Details : AccountDetailsDto
} with
    static member details_ : Lens<AccountDto, AccountDetailsDto> =
        (fun p -> p.Details), (fun x p -> {p with Details = x})

//
// Tests
//
let testInlineNesting =
    let account = Some { Details = { Tag = "Test"; InsurancePolicyData = None }}
    let details_ = Option.value_ >?> AccountDto.details_ >?> AccountDetailsDto.insurancePolicyData_

    let inline getP (optic : Prism<InsurancePolicyDetailsDto,'a>) = account^.(details_ >?> optic) // causes type error
    //let inline getP (optic : Prism<InsurancePolicyDetailsDto,'a>) = Optic.get (details_ >?> optic) account // doesn't cause type error

    let debug_getP =
        try
            getP InsurancePolicyDetailsDto.discount_
            |> Ok
        with
        | x ->
            x.ToString() |> Error

    printfn "testMultipleInlineNesting: %A" debug_getP
    Assert.AreEqual (debug_getP, Ok None)

