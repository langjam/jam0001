export const range = (N: number, from=0, step=1): number[] => 
	[...Array(N)].map((_, i) => from + i * step)