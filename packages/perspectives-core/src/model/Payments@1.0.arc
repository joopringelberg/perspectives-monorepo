domain model://perspectives.domains#Payments@1.0
  use sys for model://perspectives.domains#System
  use util for model://perspectives.domains#Utilities
  use pay for model://perspectives.domains#Payments

  -------------------------------------------------------------------------------
  ---- SETUP
  -------------------------------------------------------------------------------
  state ReadyToInstall = exists sys:PerspectivesSystem$Installer
    on entry
      do for sys:PerspectivesSystem$Installer
        letA
          app <- create context PaymentsApp
          start <- create role StartContexts in sys:MySystem
        in
          bind_ app >> extern to start
          Name = "Payments" for start
          IsSystemModel = false for start

  on exit
    do for sys:PerspectivesSystem$Installer
      letA
        indexedcontext <- filter sys:MySystem >> IndexedContexts with filledBy (pay:MyPaymentsApp >> extern)
        startcontext <- filter sys:MySystem >> StartContexts with filledBy (pay:MyPaymentsApp >> extern)
      in
        remove context indexedcontext
        remove role startcontext

  aspect user sys:PerspectivesSystem$Installer

  -------------------------------------------------------------------------------
  ---- PAYMENTS APP
  -------------------------------------------------------------------------------
  case PaymentsApp
    indexed pay:MyPaymentsApp
    aspect sys:RootContext
    external
      aspect sys:RootContext$External

    state HasNoAccountHolder = not exists RekeningHouder filledBy sys:Me
      on entry
        do for Manager
          letA
            accountHolder <- create role RekeningHouder
          in
            bind_ sys:Me to accountHolder
            PspProvider = "adyen" for accountHolder
            DefaultCurrency = "EUR" for accountHolder

    user Manager = sys:Me
      perspective on RekeningHouder
        only (Create, Fill)
        props (ReceiverAccount, PspProvider, DefaultCurrency) verbs (Consult, SetPropertyValue)

      perspective on OntvangenBetaalverzoeken
        only (Create, Fill, Remove)
        props (Amount, Currency, ReceiverAccount, PaymentReference, PspProvider, PaymentStatus, PaidAt) verbs (Consult)

      perspective on EigenBetaalverzoeken
        only (CreateAndFill, Remove)
        props (Amount, Currency, ReceiverAccount, PaymentReference, PspProvider, PaymentStatus, PaidAt) verbs (Consult)

        action MaakBetaalverzoek
          letA
            request <- create context BetaalVerzoek bound to EigenBetaalverzoeken
            ontvanger <- create role Ontvanger in request >> binding >> context
          in
            bind_ sys:Me to ontvanger
            PaymentReference = callExternal util:GenSym() returns String for request >> binding >> context >> extern
            Currency = currentcontext >> RekeningHouder >> DefaultCurrency for request >> binding >> context >> extern
            PspProvider = currentcontext >> RekeningHouder >> PspProvider for request >> binding >> context >> extern
            ReceiverAccount = currentcontext >> RekeningHouder >> ReceiverAccount for request >> binding >> context >> extern
            PaymentStatus = "Requested" for request >> binding >> context >> extern

      perspective on OpenOntvangenBetaalverzoeken
        only (Consult)
        props (Amount, Currency, ReceiverAccount, PaymentReference, PspProvider, PaymentStatus, PaidAt) verbs (Consult)

      perspective on OpenEigenBetaalverzoeken
        only (Consult)
        props (Amount, Currency, ReceiverAccount, PaymentReference, PspProvider, PaymentStatus, PaidAt) verbs (Consult)

      screen "Payments"
        tab "Openstaande ontvangen verzoeken" default
          table OpenOntvangenBetaalverzoeken
        tab "Openstaande eigen verzoeken"
          table OpenEigenBetaalverzoeken
        tab "Alle ontvangen verzoeken"
          table OntvangenBetaalverzoeken
        tab "Alle eigen verzoeken"
          table EigenBetaalverzoeken
        tab "Rekeninghouder"
          form RekeningHouder

    user RekeningHouder filledBy sys:PerspectivesSystem$User
      property ReceiverAccount (String)
      property PspProvider (String)
      property DefaultCurrency (String)

    context OntvangenBetaalverzoeken (relational) filledBy BetaalVerzoek
    context EigenBetaalverzoeken (relational) filledBy BetaalVerzoek

    context OpenOntvangenBetaalverzoeken = filter OntvangenBetaalverzoeken with binding >> context >> extern >> IsOpen
    context OpenEigenBetaalverzoeken = filter EigenBetaalverzoeken with binding >> context >> extern >> IsOpen

  -------------------------------------------------------------------------------
  ---- BETAALVERZOEK
  -------------------------------------------------------------------------------
  case BetaalVerzoek
    external
      property Amount (Number)
      property Currency (String)
      property ReceiverAccount (String)
      property PaymentReference (String)
      property PspProvider (String)
      property PspPaymentId (String)
      property PspReturnPayload (String)
      property PaymentStatus (String)
        enumeration = ("Requested", "Pending", "Paid", "Failed", "Expired", "Cancelled")
      property PaidAt (DateTime)
      property VerificationDetails (String)
      property IsOpen = not (PaymentStatus == "Paid" or PaymentStatus == "Failed" or PaymentStatus == "Expired" or PaymentStatus == "Cancelled")

      perspective of Ontvanger
        props (Amount, Currency, ReceiverAccount, PaymentReference, PspProvider, PspPaymentId, PspReturnPayload, PaymentStatus, PaidAt, VerificationDetails, IsOpen) verbs (Consult, SetPropertyValue)

      perspective of Betaler
        props (Amount, Currency, ReceiverAccount, PaymentReference, PspProvider, PspPaymentId, PspReturnPayload, PaymentStatus, PaidAt, VerificationDetails, IsOpen) verbs (Consult, SetPropertyValue)

    user Ontvanger filledBy sys:PerspectivesSystem$User
      perspective on Betaler
        only (Create, Fill)
        props (LastName) verbs (Consult)

    user Betaler filledBy sys:PerspectivesSystem$User
      perspective on Ontvanger
        only (Consult)
        props (LastName) verbs (Consult)
