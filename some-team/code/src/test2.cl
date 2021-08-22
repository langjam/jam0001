@thread
fn some_thread() {
    let i = 0;
    for {
        let work = @publish_work(i);
        for {
            let comment = @rfc(work);
            if (comment == :main_ack) {
                break;
            }
            @log("Comment: *", comment);
        }
        i += 1;
    }
}

@thread
fn main() {
    for {
        let work = @seework("some_thread");
        @comment(work, :main_ack);
        @log("some_thread's work: *", work);
    }
}