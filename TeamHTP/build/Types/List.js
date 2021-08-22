import Base from './Base';
import { mdastListItemToMd } from '../Markdown';
class List extends Base {
    getMdastContent() {
        return this.mdastContent;
    }
    /**
     * Checks the first item of the list with the given name.
     * @param itemName
     * @returns ListItem that was updated, or undefined if no item with the given name was found in the list.
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
}
export default List;
