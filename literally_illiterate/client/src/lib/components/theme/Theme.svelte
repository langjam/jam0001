<script lang='ts'>
    import { COLORS } from '$lib/constants/colors'
    import { setCSSVar } from '$lib/utils'    
    import { Theme } from '@ollopa/cedar'
    import { onMount } from 'svelte'

    let themeRoot: HTMLElement

    onMount(() => Object.entries(COLORS).forEach(setCSSVar(themeRoot)))
</script>

<Theme>
    <div class='theme' bind:this={themeRoot}>
        <slot />
    </div>
</Theme>

<style>
    .theme {
        font-family: var(--bodyFont);   
        background: var(--backgroundColor);
    }

    :global(.theme) {
        /* Element Spacing */
        --stickyHeaderHeight: 44px;
        
        /* Borders */
        --borderRadius: 16px;
        --borderRadiusSmall: 12px;
        --borderRadiusFull: 999px;
        --buttonBorderRadius: var(--borderRadiusFull);
        --line: 1.5px solid var(--lineColor);
        --lineThin: 1px solid var(--lightLineColor);
        --lineMedium: 2px solid var(--lightLineColor);
        --lineThick: 3px solid var(--lightLineColor);
        --lineThicker: 4.5px solid var(--lightLineColor);
        --hairline: 1px solid var(--hairLineColor);
        
        /* Typography */
        --headingFont: 'Open Sans', serif;
        --bodyFont: 'Open Sans', sans-serif;
        --monoFont: 'IBM Plex Mono', monospace;   
        --weightMedium: 500;
        --weightBold: 600;
        --weightBolder: 700;
     
        /* Minor Third */        
        --h1: 1.802rem;
        --h2: 1.602rem;
        --h3: 1.424rem;
        --h4: 1.266rem;
        --h5: 1.125rem;
        --textSmall: 0.889rem;
        --textSmaller: 0.7rem;
        /* View Paddings */
        --maxModalWidth: 80vw;
        --maxModalHeight: 90vh;
        --modalPaddingX: var(--s-16);
        --modalPaddingY: var(--s-16);
    }    

    :global(table) {
        background: var(--tableColor);
        border: var(--line);
        border-collapse: collapse;
        border-color: var(--tableBorderColor);
    }
    :global(td) {
        border: var(--lineThin);
        border-color: var(--tableCellBorderColor);
        padding: 0 var(--s-2);
		text-align: center;
    }
    :global(thead) {
        background: var(--tableHeaderColor);
        color: var(--tableHeaderTextColor);
    }

    :global(h1), :global(h2), :global(h3), :global(h4), :global(h5), :global(h6) {
        font-weight: var(--weightBold);
    }

    /**
    * Via Mozilla:
    *   The user-select CSS property controls whether the user can select text. 
    *   This doesn't have any effect on content loaded as chrome, except in textboxes.
    */
    /* :global(input, textarea) {
        -webkit-user-select: auto !important;
        -khtml-user-select: auto !important;
        -moz-user-select: auto !important;
        -ms-user-select: auto !important;
        user-select: auto !important;
    } */

    /* ---------------------------------------------------- DATA TOOL TIP */
    :global([data-tooltip]) {
        position: relative;
        z-index: 2;
        display: block;
    }

    :global([data-tooltip]:before),
    :global([data-tooltip]:after) {
        visibility: hidden;
        opacity: 0;
        pointer-events: none;
            transition: .2s ease-out;
            transform: translate(-50%, 5px)
    }

    :global([data-tooltip]:before) {
        position: absolute;
        bottom: 100%;
        left: 50%;
        margin-bottom: 5px;
        padding: 7px;
        width: 100%;
        min-width: 70px;
        max-width: 250px;
        -webkit-border-radius: 3px;
        -moz-border-radius: 3px;
        border-radius: 3px;
        background-color: #000;
        background-color: hsla(0, 0%, 20%, 0.9);
        color: #fff;
        content: attr(data-tooltip);
        text-align: center;
        font-size: 14px;
        line-height: 1.2;
            transition: .2s ease-out
    }

    :global([data-tooltip]:after) {
        position: absolute;
        bottom: 100%;
        left: 50%;
        width: 0;
        border-top: 5px solid #000;
        border-top: 5px solid hsla(0, 0%, 20%, 0.9);
        border-right: 5px solid transparent;
        border-left: 5px solid transparent;
        content: " ";
        font-size: 0;
        line-height: 0;
    }

    :global([data-tooltip]:hover:before),
    :global([data-tooltip]:hover:after) {
        visibility: visible;
        opacity: 1;
            transform: translate(-50%, 0)
    }
    :global([data-tooltip=false]:hover:before),
    :global([data-tooltip=false]:hover:after) {
        visibility: hidden;
        opacity: 0;
    }
    /* ---------------------------------------------------- END DATA TOOL TIP */
</style>
