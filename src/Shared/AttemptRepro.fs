module AttemptRepro

open Optics
open Optics.Operators

type PaymentFrequencyDto =
    | Weekly = 1
    | Fortnightly = 2
    | Monthly = 3
    | Quarterly = 4
    | Annual = 5

type InsurancePolicyDetailsDto = {
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

type AccountDetailsDto = {
    Tag : string
    InsurancePolicyData : InsurancePolicyDetailsDto option
} with
    static member insurancePolicyData_ : Prism<AccountDetailsDto, InsurancePolicyDetailsDto> =
        (fun p -> p.InsurancePolicyData),
        (fun x p ->
            match p with
            | p when p.InsurancePolicyData.IsSome -> {p with InsurancePolicyData = Some x}
            | p -> p)


type AccountDto = {
    Details : AccountDetailsDto
} with
    static member details_ : Lens<AccountDto, AccountDetailsDto> =
        (fun p -> p.Details), (fun x p -> {p with Details = x})

let AttemptRepro (currentValue : AccountDto option)=
    let details_ = Option.value_ >?> AccountDto.details_ >?> AccountDetailsDto.insurancePolicyData_


    //let inline getL (optic : Lens<InsurancePolicyDetailsDto,'a>) = Optic.get (details_ >?> optic) currentValue
    //let inline getP (optic : Prism<InsurancePolicyDetailsDto,'a>) = Optic.get (details_ >?> optic) currentValue
    // This compiles OK. But comment the above two lines out, and uncomment the ones below and you'll get the
    // "Cannot resolve trait call op_HatDot - Inline call from " errors, but it *also* causes new compile time errors on
    // AttemptRepro.fs lines 38, 43 and 48.
    let inline getL (optic : Lens<InsurancePolicyDetailsDto,'a>) = currentValue^.(details_ >?> optic)
    let inline getP (optic : Prism<InsurancePolicyDetailsDto,'a>) = currentValue^.(details_ >?> optic)

    let lensFirst = getL InsurancePolicyDetailsDto.paymentFrequency_ |> Option.toList
    printfn "lensFirst = %A" lensFirst
    let debug_getP = getP InsurancePolicyDetailsDto.discount_
    let debug_cmp : decimal option = None
    let debug_cmp2 : decimal option = None
    printfn "debug_getP == debug_cmp = %A" (debug_getP = debug_cmp)
    printfn "debug_cmp == debug_cmp2 = %A" (debug_cmp = debug_cmp2)
    printfn "...returned %A where type = %A (vs %A where type = %A)..." debug_getP debug_getP.GetType debug_cmp debug_cmp.GetType
    debug_getP