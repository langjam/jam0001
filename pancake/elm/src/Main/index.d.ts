export namespace Elm {
    namespace Main {
      export interface App {
        ports: {
            compile: { send(code: string): void },
            result: { subscribe(listener: (r: any) => void) },
            step: { send(dummy: boolean): void },
            state: { subscribe(listener: (s: any) => void) }
        };
      }
      export function init(): Elm.Main.App;
    }
  }