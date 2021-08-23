export const setCSSVar = (element = document.documentElement) => ([name, value]: [string, string]): void =>
    element.style.setProperty(`--${name}`, value)

export const getCSSVar = (name: string): string =>
    getComputedStyle(document.body).getPropertyValue(`--${name}`)

export const getCSSVarPx = (name: string): number =>
    parseInt(getCSSVar(name).split('px')[0])