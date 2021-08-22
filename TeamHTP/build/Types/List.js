import Base from './Base';
import { mdastListItemToMd, mdToMdast } from '../Markdown';
class List extends Base {
    getMdastContent() {
        return this.mdastContent;
    }
    /**
     * Checks the first unchecked item of the list with the given name.
     * @param itemName
     * @returns ListItem that was updated, or undefined if no unchecked item with the given name was found in the list.
     */
    checkItem(itemName) {
        for (const listItem of this.getMdastContent().children) {
            const listItemMd = mdastListItemToMd(listItem);
            if (itemName === listItemMd && !listItem.checked) {
                listItem.checked = true;
                return listItem;
            }
        }
        return undefined;
    }
    /**
     * Unchecks the first checked item of the list with the given name.
     * @param itemName
     * @returns ListItem that was updated, or undefined if no checked item with the given name was found in the list.
     */
    uncheckItem(itemName) {
        for (const listItem of this.getMdastContent().children) {
            const listItemMd = mdastListItemToMd(listItem);
            if (itemName === listItemMd && listItem.checked) {
                listItem.checked = false;
                return listItem;
            }
        }
        return undefined;
    }
    /**
     * Adds an item to the list at the given index
     * @param index index to add
     * @param item a markdown element
     */
    addItem(item, index) {
        const isChecked = item.trimStart().startsWith('[x] ');
        const isUnchecked = item.trimStart().startsWith('[ ] ');
        const isCheckbox = isChecked || isUnchecked;
        if (isCheckbox) {
            item = item.replace(/\[(x| )] /, '');
        }
        const element = mdToMdast(item).children[0];
        const listItem = {
            children: [element], data: undefined, type: 'listItem', checked: isCheckbox ? isChecked : undefined
        };
        if (index !== undefined) {
            this.getMdastContent().children.splice(index, 0, listItem);
        }
        else {
            this.getMdastContent().children.push(listItem);
        }
    }
    /**
     * Adds an item to the list at the given index
     * @param index index to add
     * @param item a markdown element
     */
    removeItem(item) {
        const isChecked = item.trimStart().startsWith('[x] ');
        const isUnchecked = item.trimStart().startsWith('[ ] ');
        const isCheckbox = isChecked || isUnchecked;
        for (let i = 0; i < this.getMdastContent().children.length; i++) {
            const child = this.getMdastContent().children[i];
            if (mdastListItemToMd(child) === item) {
                this.removeItemAtIndex(i);
                break;
            }
        }
    }
    removeItemAtIndex(index) {
        this.getMdastContent().children.splice(index, 1);
    }
    isChecked(itemName) {
        for (const listItem of this.getMdastContent().children) {
            const listItemMd = mdastListItemToMd(listItem);
            if (itemName === listItemMd && listItem.checked !== undefined) {
                return listItem.checked;
            }
        }
    }
}
export default List;
