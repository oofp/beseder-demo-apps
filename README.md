# beseder-demo-apps

## ATM control program
This demo implements control of simplified ATM. 
ATM consists of four resources:
* [Card reader](https://github.com/oofp/beseder-demo-apps/blob/master/atm-demo/src/Beseder/Atm/Resources/CardReaderRes.hs)
* [Account](https://github.com/oofp/beseder-demo-apps/blob/master/atm-demo/src/Beseder/Atm/Resources/AccountRes.hs)
* [Cash dispenser](https://github.com/oofp/beseder-demo-apps/blob/master/atm-demo/src/Beseder/Atm/Resources/CashDispenserRes.hs)
* [Terminal](https://github.com/oofp/beseder-demo-apps/blob/master/atm-demo/src/Beseder/Atm/Resources/TerminalRes.hs)

Supported operations:
* Checking account balance
* Cash withdrawal

Currently there are two alternative implementations of the program:
* [sequentional (step by step) computatition](https://github.com/oofp/beseder-demo-apps/blob/master/atm-demo/src/Beseder/Atm/AtmApp.hs

* [event loop](https://github.com/oofp/beseder-demo-apps/blob/master/atm-demo/src/Beseder/Atm/AtmLoopApp.hs)

The demo are using Beseder feature of deriving resource declaration from type class that describe resource behavior. As a bonus, it also creates state diagram that provides visual presentation of resource behavior.

### Here are state diagram for CashDispenser and Terminal resources:


![Cash Dispenser Resource](https://user-images.githubusercontent.com/25211514/70852603-5be39280-1e71-11ea-88e5-e4f6e403299d.png)

###

![ATM Terminal Resource](https://user-images.githubusercontent.com/25211514/70852346-67818a00-1e6e-11ea-854d-ecbd5201939b.png)

### And here is entire state diagram (probably a bit to detailed) extracted from ![AtmApp.hs](https://github.com/oofp/beseder-demo-apps/blob/master/atm-demo/src/Beseder/Atm/AtmApp.hs)

![ATM State Diagram](https://user-images.githubusercontent.com/25211514/71395053-52c99280-25e2-11ea-9eb4-dd4412696791.png)
