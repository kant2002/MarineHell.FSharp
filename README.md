# F# bot for Starcraft BroodWar: MarineHell 

Port of https://github.com/libor-vilimek/marine-hell to F# using [BWAPI.NET](https://github.com/acoto87/bwapi.net)

## Quick Start

1. Installation
    * Install [.NET SDK](https://dotnet.microsoft.com/en-us/download)
    * Install **StarCraft: Brood War**
    * Update **StarCraft: Brood War to 1.16.1**
    * Install [BWAPI](https://bwapi.github.io/)
2. Run project
    * Run `dotnet run` (At this point you should see _"Game table mapping not found."_ printed each second)
3. Run StarCraft through **Chaoslauncher**
    * Run _Chaoslauncher.exe_ as administrator
        * Chaoslauncher is found in Chaoslauncher directory of [BWAPI](https://bwapi.github.io/) install directory
    * Check the _BWAPI Injector x.x.x [RELEASE]_
    * Click Start
        * Make sure the version is set to Starcraft 1.16.1, not ICCup 1.16.1
4. Run a game against Blizzard's AI
    * Go to **Single Player** -> **Expansion**
    * Select any user and click **OK**
    * Click **Play Custom**, select a map, and start a game
5. Run a game against yourself
    * Run _Chaoslauncher - MultiInstance.exe_ as administrator
    * Start
        * Go to **Multiplayer** -> **Expansion** -> **Local PC**
        * Select any user and click **OK**
        * Click **Create Game**, select a map, and click **OK**
    * Start – Uncheck _BWAPI Injector x.x.x [RELEASE]_ to let a human play, leave alone to make AI play itself
        * Go to **Multiplayer** -> **Expansion** -> **Local PC**
        * Select any user and click **OK**
        * Join the existing game created by the other client

