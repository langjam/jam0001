use crate::frontend::{FrontEnd, Frontend, Message};
use warp::Filter;

pub struct WebFrontend {

}

impl Frontend for WebFrontend {
    fn receive(&self, m: Message) {
        todo!()
    }
}

impl WebFrontend {
    pub fn new() {

    }
}