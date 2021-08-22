import Base from './Base';
class Heading extends Base {
    getMdastContent() {
        return this.mdastContent;
    }
    depth(depth) {
        if (depth) {
            this.getMdastContent().depth = depth;
        }
        return this.getMdastContent().depth;
    }
}
export default Heading;
