const BASE = {
    trueWhite: '#FFFFFF',
    trueBlack: '#000000',

    white: '#FFFFFF',
    lightestGray: '#F9FAFB',
    lighterGray: '#F1F2F6',
    lightGray: '#EBEEF2',
    gray: '#C1C5CD',
    darkGray: '#8D929F',
    darkerGray: '#535A6B',
    black: '#09132C',    

    lightBlue: '#CFEEFF',
    blue: '#0B90FF',
    darkBlue: '#069BEC',

    lightestBlueGray: '#F5F8FB',
    lightBlueGray: '#F2F6FA',
    blueGray: '#D9DFE8',

    lightRed: '#ffbcad',
    red: '#FE3957',

    lightGreen: '#DDF3E3',
    green: '#0CAF82',
    darkGreen: '#70CB89',
    darkestGreen: '#56A66C',

    gold: '#FFE6B5',
    darkGold: '#ec7c05',

    purple: '#C1C8FF',
    darkPurple: '#5968ED',

    orange: '#FEA95A',
    darkOrange: '#FE8414',
}

const ATOM_COLORS = {
    // LINES
    hairLineColor: BASE.lighterGray,
    lineColor: BASE.lighterGray,
    lightLineColor: BASE.lightestGray,
}

const ELEMENT_COLORS = {
    backgroundColor: BASE.lightGray,

    // SECTIONS
    sectionBackgroundColor: BASE.white,
    sectionHeaderBackgroundColor: BASE.lightestGray,
    sectionHeaderBorderColor: BASE.lightGray,

    // Used in the FadeScroll element (can't apply alpha to hex in CSS)
    backgroundRgb: [241, 240, 242],
    blockBg: BASE.white,
    contextMenuBg: BASE.gray,

    // STICKY HEADER
    stickyHeaderColor: BASE.lightBlueGray,
    stickyHeaderBorderColor: BASE.blueGray,

    // TABLES
    tableHeaderColor: BASE.lightestGray,
    tableHeaderTextColor: BASE.darkGray,
    tableColor: BASE.white,
    tableCellBorderColor: ATOM_COLORS.lineColor,
    tableBorderColor: BASE.gray,
}

const TEXT_COLORS = {
    selectedText: BASE.blue,
    text: BASE.black,
    lightText: BASE.darkGray,
    lighterText: BASE.gray,
}

export const COLORS = {
    ...ATOM_COLORS,
    ...BASE,
    ...TEXT_COLORS,
    ...ELEMENT_COLORS,
}