import { readFileSync } from 'fs';
import pkg from 'ts-jest/utils/index.js';
const { pathsToModuleNameMapper } = pkg;

const { compilerOptions } = JSON.parse(readFileSync('./tsconfig.json'))

export default {
	preset: 'ts-jest',
	testEnvironment: 'node',
	testMatch: ['<rootDir>/**/*.test.ts'],
	testPathIgnorePatterns: ['/node_modules/'],
	coverageDirectory: './coverage',
	coveragePathIgnorePatterns: ['node_modules'],
	globals: { 'ts-jest': { diagnostics: false } },
	transform: {},
	moduleNameMapper: pathsToModuleNameMapper(compilerOptions.paths, { prefix: '<rootDir>/' }),
}